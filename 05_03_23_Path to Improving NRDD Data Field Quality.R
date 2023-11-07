## Path to Improving NRDD Data Quality

# Author: Francis Choi and Ishrat Jabin

rm(list= ls())
library(tidyverse)
library(tableHTML)
library(lubridate)
library(janitor)
library(rvest)

#load project info table
setwd('C:/Users/ishrat.jabin/Documents/NRDD QC/Update Dashboard 05_03_23/Source Data')
htm_tbl <- read_html("QueryBuilder_Project_Information_06_07_23_Approved.HTM") #via HTML
info <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
info <- info %>% clean_names() %>%
  distinct(nrdd_project_id, .keep_all = TRUE)

  # mutate(last_record_update = as.Date(mdy_hms(last_record_update))) %>%
  # mutate(planned_project_start = mdy(planned_project_start)) %>%
  # mutate(planned_project_end = mdy(planned_project_end)) %>%
  # mutate(actual_project_start = mdy(actual_project_start)) %>%
  # mutate(actual_project_end = mdy(actual_project_end))

raw <- info

info$last_update_yr <- format(as.Date(info$last_record_update, format="%m/%d/%Y"), "%Y")

#put NULL to help analysis
info[info == ""] <- "NULL"
info[is.na(info)] <- "NULL"
info[info == "No PI Affiliation set"] <- "NULL"

#################################
##project information datasheet## 
################################################################################
# Use this as reference to determine which fields were being indexed for des_cols.
# NRDD Project Info Data Fields as of 06/08/23
# Fields that are skipped will either not be analyzed for quality
# OR be analyzed later in the process

#1  = nrdd_project_id (skip)  | #11 = current_rl_description (skip)      | #21 = actual_project_end (skip)            | #31 = noaa_poc_last_name                    | #41 = alternate_project_id_type      
#2  = nrdd_status   (skip)    | #12 = current_rl_category (skip)         | #22 = pi_first_name                        | #32 = noaa_poc_email                        | #42 = current_project_status (skip)
#3  = project_title           | #13 = transition_plan_y_n (skip)         | #23 = pi_last_name                         | #33 = noaa_poc_affiliation                  | #43 = record_creator (skip)
#4  = lo                      | #14 = r2a (skip)                         | #24 = pi_email                             | #34 = noaa_poc_phone                        | #44 = source_database (skip)
#5  = office                  | #15 = r2c (skip)                         | #25 = pi_affiliation                       | #35 = x2018_disaster_supplemental (skip)    | #45 = last_record_update (skip)
#6  = division                | #16 = r2o (skip)                         | #26 = pi_affiliation_state_country (skip)  | #36 = economic_valuation (skip)             | #46 = updated_by (skip)
#7  = project_description     | #17 = r2x (skip)                         | #27 = pi_affiliation_scale (skip)          | #37 = x2018_disaster_supplemental (skip)    | 
#8  = project_benefits        | #18 = planned_project_start              | #28 = pi_affiliation_type  (skip)          | #38 = x2021_IIJA_law  (skip)                | 
#9  = project_outcome         | #19 = planned_project_end                | #29 = pi_phone                             | #39 = x2022_inflation_reduction_act (skip)  | 
#10 = current_rl              | #20 = actual_project_start (skip)        | #30 = noaa_poc_first_name                  | #40 = alternate_project_id                  |          


#dataframe for completeness for descriptive fields------------------------------
missing_data <- data.frame(nrdd_project_id = info$nrdd_project_id, lo_name = info$lo, office_name = info$office,  yr_updated = info$last_update_yr)
count <- 1
#Select anything not in (skip)
des_cols <- c(3:10,18:19,22:25,29:34,40:41)
for (i in 1:(length(des_cols)))
{
  temp <- des_cols[i] 
  temp_vector <- ifelse(info[,temp] == "NULL", 0, 1)
  missing_data <- cbind(missing_data, temp_vector)
  count <- count + 1
}  
# these columns will be used to sort the data quality dashboard
colnames(missing_data) <- c("nrdd_project_id", "lo_name", "office_name", "yr_updated", colnames(info[des_cols]))
#view(missing_data)

#list of percent of completeness for descriptive fields

#first value of info.null is the number of projects-----------------------------
# info.null <- 0
# info.null[1] <- length(info$nrdd_project_id)
# count <- 2
# info.null[2:(length(des_cols)+1)] <- as.integer(colSums(missing_data[, -(1:(length(missing_data)-length(des_cols)))])/length(info$nrdd_project_id)*100)
# header <- c("sum", colnames(info[des_cols]))
# missing_data_list <- data.frame(field = header, overall = info.null)

#view(missing_data_list)

#analysis on project description, benefits, outcomes----------------------------
#see if description and benefit/outcome are the same
des.ben <- ifelse(info$project_description == info$project_benefits, "yes", "no")
des.out <- ifelse(info$project_description == info$project_outcome, "yes", "no")
ben.out <- ifelse(des.ben == "yes", ifelse(des.out == "yes", "yes", "no"),"no")
temp.qc.des <- NA
temp.qc.ben <- NA
temp.qc.out <- NA
temp.des <- 0
temp.ben <- 0
temp.out <- 0
for (i in 1: (as.numeric(length(info$nrdd_project_id))))
{
  if (des.ben[i] == "yes"){
    temp.qc.des[i] <- "description and benefits are the same"
    temp.qc.ben[i] <- "description and benefits are the same"
    temp.des[i] <- 1
    temp.ben[i] <- 0
  } else {
    temp.qc.des[i] <- "fine"
    temp.qc.ben[i] <- "fine"
    temp.des[i] <- 1
    temp.ben[i] <- 1
  }
  if (des.out[i] == "yes"){
    temp.qc.des[i] <- "description and outcomes are the same"
    temp.qc.out[i] <- "description and outcomes are the same"
    temp.des[i] <- 1
    temp.out[i] <- 0
  } else {
    temp.qc.out[i] <- "fine"
    temp.out[i] <- 1
  }  
  if (ben.out[i] == "yes"){
    temp.qc.des[i] <- "description, benefits and outcomes are the same"
    temp.qc.ben[i] <- "description, benefits and outcomes are the same"
    temp.qc.out[i] <- "description, benefits and outcomes are the same"
    temp.des[i] <- 1
    temp.ben[i] <- 0
    temp.out[i] <- 0
  }  
}

missing_data$project_description <- temp.des
missing_data$project_benefits <- temp.ben
missing_data$project_outcome <- temp.out
proj.info.qc <- data.frame(nrdd_project_id = info$nrdd_project_id, lo = info$lo, yr_updated = info$last_update_yr, project_description = temp.qc.des, project_benefits = temp.qc.ben, project_outcome = temp.qc.out)

#analysis completeness for actual start, actual end, project status
#actual start
temp.qc <- NA
temp.com <- 1
for (i in 1: (as.numeric(length(info$nrdd_project_id))))
{
  if (info$actual_project_start[i] == "NULL") {
    if(info$actual_project_end[i] != "NULL") {
      temp.qc[i] <- "actual start end don't match"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "In Progress") {
      temp.qc[i] <- "inconsistent"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Completed") {
      temp.qc[i] <- "inconsistent"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Transitioned") {
      temp.qc[i] <- "inconsistent"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Pending Transition") {
      temp.qc[i] <- "inconsistent"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Pending Deliverables") {
      temp.qc[i] <- "inconsistent"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "NULL") {
      temp.qc[i] <- "no reliable info"
      temp.com[i] <- 0
    } else {
      temp.qc[i] <- "nothing to see here"
      temp.com[i] <- 1
    }
  }
   else if(info$actual_project_start[i] != "NULL") {
     if(info$current_project_status[i] == "Planned") {
      temp.qc[i] <- "Error"
      temp.com[i] <- 0 
   } else if(info$current_project_status[i] == "Unfunded") {
      temp.qc[i] <- "Error"
      temp.com[i] <- 0 
   } else {
     temp.qc[i] <- "Actual Start Date is there"
     temp.com[i] <- 1
   }
  }
}
missing_data$actual_project_start <- temp.com
#temp_missing_list <- data.frame(field = "actual_project_start", overall = as.integer((sum(temp.com)/missing_data_list$overall[1])*100)  )
#missing_data_list <- rbind(missing_data_list, temp_missing_list)
proj.info.qc$actual_project_start <- temp.qc

# Analysis on Actual END Date -------------------------------------------------
temp.qc <- NA
temp.com <- 1
for (i in 1: (as.numeric(length(info$nrdd_project_id))))
{
  # If there is no end date (null)
  if (info$actual_project_end[i] == "NULL") {
    if(info$current_project_status[i] == "Completed"){
      temp.qc[i] <- "Need End Date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Transitioned") {
      temp.qc[i] <- "Need End Date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Pending Transitioned") {
      temp.qc[i] <- "Need End Date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Pending Deliverables") {
      temp.qc[i] <- "Need End Date"
      temp.com[i] <- 0
    }  else if(info$current_project_status[i] == "Cancelled") {
      temp.qc[i] <- "Need End Date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "NULL") {
      temp.qc[i] <- "unsure"
      temp.com[i] <- 0
    } else {
      temp.qc[i] <- "nothing to see here"
      temp.com[i] <- 1
    }
  } 
  # If there is an end date (not null)
  else if(info$actual_project_end[i] != "NULL"){
    if(info$current_project_status[i] == "Unfunded"){
      temp.qc[i] <- "should not have end date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Planned") {
      temp.qc[i] <- "should not have end date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "In Progress") {
      temp.qc[i] <- "should not have end date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "Delayed") {
      temp.qc[i] <- "should not have end date"
      temp.com[i] <- 0
    } else if(info$current_project_status[i] == "On hold") {
      temp.qc[i] <- "should not have end date"
      temp.com[i] <- 0
    } else{
      temp.qc[i] <- "everything looks good"
      temp.com[i] <- 1
    }
  } 
}
missing_data$actual_project_end <- temp.com
#temp_missing_list <- data.frame(field = "actual_project_end", overall = as.integer((sum(temp.com)/missing_data_list$overall[1])*100)  )
#missing_data_list <- rbind(missing_data_list, temp_missing_list)
proj.info.qc$actual_project_end <- temp.qc

# Analysis on Current Project Status ------------------------------------------
temp.qc <- NA
temp.com <- 0
for (i in 1: (as.numeric(length(info$nrdd_project_id))))
{
  if(info$current_project_status[i] == "NULL"){
    temp.qc[i] <- "status cannot be empty"
    temp.com[i] <- 0
  } 
  else if(info$current_project_status[i] == "Unfunded"){
    # should NOT have start or End Dates
    if(info$actual_project_start[i] != "NULL" | info$actual_project_end[i] != "NULL"){
      temp.qc[i] <- "this status can't have actual start or end dates"
      temp.com[i] <- 0
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1      
    }
  } 
  else if(info$current_project_status[i] == "Planned"){
    if(info$actual_project_start[i] != "NULL" | info$actual_project_end[i] != "NULL"){
      temp.qc[i] <- "this status can't have actual start or end dates"
      temp.com[i] <- 0
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1      
    }
  } else if(info$current_project_status[i] == "In Progress"){
    if(info$actual_project_start[i] == "NULL"){
      temp.qc[i] <- "this status must have actual start"
      temp.com[i] <- 0      
    } else if(info$actual_project_end[i] != "NULL"){
      temp.qc[i] <- "this status can't have actual end"
      temp.com[i] <- 0          
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1             
    }
  } else if(info$current_project_status[i] == "Completed"){
    if(info$actual_project_start[i] == "NULL" | info$actual_project_end[i] == "NULL"){
      temp.qc[i] <- "needs Actual Start and End Dates"
      temp.com[i] <- 0    
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1                   
    }
  } else if(info$current_project_status[i] == "Transitioned"){
    if(info$actual_project_start[i] == "NULL" | info$actual_project_end[i] == "NULL"){
      temp.qc[i] <- "needs Actual Start and End Dates"
      temp.com[i] <- 0    
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1                   
    }
  } else if(info$current_project_status[i] == "Pending Transition") {
    if(info$actual_project_start[i] == "NULL" | info$actual_project_end[i] == "NULL") {
      temp.qc[i] <- "needs Actual Start and End Dates"
      temp.com[i] <- 0                              
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1                               
    }
  } else if(info$current_project_status[i] == "Pending Deliverable"){
    if(info$actual_project_start[i] == "NULL" | info$actual_project_end[i] == "NULL") {
      temp.qc[i] <- "needs Actual Start and End Dates"
      temp.com[i] <- 0                                          
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1                               
    }
  } else if(info$current_project_status[i] == "Delayed"){
    if(info$actual_project_end[i] != "NULL") {
      temp.qc[i] <- "should not have end date"
      temp.com[i] <- 0                                          
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1                               
    }
  } else if(info$current_project_status[i] == "On hold"){
    if(info$actual_project_end[i] != "NULL") {
      temp.qc[i] <- "should not have end date"
      temp.com[i] <- 0                                          
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1                               
    }
  } else if(info$current_project_status[i] == "Cancelled"){
    if(info$actual_project_end[i] == "NULL") {
      temp.qc[i] <- "needs Actual End Dates"
      temp.com[i] <- 0                                          
    } else {
      temp.qc[i] <- "fine"
      temp.com[i] <- 1                               
    }
  } else {
    temp.qc[i] <- "fine"
    temp.com[i] <- 1                                   
  }  
 
}
missing_data$current_project_status <- temp.com
#temp_missing_list <- data.frame(field = "current_project_status", overall = as.integer((sum(temp.com)/missing_data_list$overall[1])*100)  )
#missing_data_list <- rbind(missing_data_list, temp_missing_list)
proj.info.qc$current_project_status <- temp.qc

# Analysis on Planned Dates Data-----------------------------------------------
plan <- time_length(as.Date(info$planned_project_end, format="%m/%d/%Y") - as.Date(info$planned_project_start, format="%m/%d/%Y"), "year")
plan_date_duration <- ifelse(plan <= 5, "0-5yrs",
                             ifelse(plan <= 10, "5-10yrs",
                                    ifelse(plan <= 20, "10-20yrs",
                                           ifelse(plan <= 50, "20-50yrs", "50yrs"))))
proj.info.qc$planned_dates <- plan_date_duration

############################
##R2X and Transition Info ##
################################################################################

# Get Transition Info Table from QueryBuilder

htm_tbl <- read_html("QueryBuilder_Transitions_06_07_23_All.HTM") #via HTML
transinfo <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
transinfo <- transinfo %>% clean_names()

# If an Admin pulls the data and it includes non-approved data, 
# RL table must be filtered to only include Approved data.
transinfo <- transinfo %>%
  filter(nrdd_project_id %in% info$nrdd_project_id)

transinfo[transinfo == ""] <- "NULL"
transinfo[is.na(transinfo)] <- "NULL"

# INDEX: nrdd_id,expected transition date, r2a, r2c, r2o, and r2x field 
rx <- transinfo[c(1,14,16:19)] %>%
  distinct(nrdd_project_id, .keep_all = TRUE)
rx$rx.qc <- 0

for (i in 1: (length(rx$nrdd_project_id)))
{
  rx$rx.qc[i] <- sum(rx[i,3:6]== "NULL")
  if (rx$rx.qc[i] == 4) {
    # If all null values assign 5.
    rx$rx.qc[i] <- 5} 
  else{
    # otherwise count the number of nos
    rx$rx.qc[i] <- sum(rx[i,3:6]== "No")
  }
}
# if rx$rx.qc[i] == 5, then there is no info.
# if rx$rx.qc[i] == 4, then that means it does not intend to transition
# if rx$rx.qc[i] < 4, then it intends to transition 

rx$rx.qc.stat <- ifelse(rx$rx.qc == 5, "No r2x info", ifelse(rx$rx.qc == 4, "No Transition", "Yes Transition"))
rx$rx.qc <- ifelse(rx$rx.qc.stat == "No r2x info", 0 , 1)
rx$exp_trans <- 5

for (i in 1:length(rx$nrdd_project_id)) {
  if (rx$rx.qc.stat[i] == "Yes Transition" & rx$expected_transition_date[i] != "NULL"){
    rx$exp_trans[i] <- 1
  } else if(rx$rx.qc.stat[i] == "No Transition" & rx$expected_transition_date[i] == "NULL") {
    rx$exp_trans[i] <- 1
  } else{
    rx$exp_trans[i] <- 0
  }
}

## analysis on external or noaa adopter
transinfo$adopter.qc <- ifelse(transinfo$external_adopter_name != "NULL" | transinfo$noaa_adopter != "NULL", 1, 0)

list <- unique(transinfo$nrdd_project_id)
temp <- NA
proj <- NA

for (i in 1: (as.numeric(length(list))))
{
  temp <- filter(transinfo, nrdd_project_id == list[i])
  if(1 %in% temp$adopter.qc ) {
    proj[i] <- "Adopter Exists"
  }else {
    proj[i] <- "No Adopter"
  }
}
adopter.qc <- data.frame(nrdd_project_id = list, adopter.comp = proj)
rx <- merge(rx, adopter.qc, by = "nrdd_project_id", all.x = TRUE)

# Check to see if transition adopter matches transition status
for (i in 1:length(rx$nrdd_project_id)) {
  if (rx$rx.qc.stat[i] == "Yes Transition" & rx$adopter.comp[i] == "Adopter Exists"){
    rx$adopter.qc[i] <- 1
  } else if(rx$rx.qc.stat[i] == "No Transition" & rx$adopter.comp[i] == "No Adopter") {
    rx$adopter.qc[i] <- 1
  } else{
    rx$adopter.qc[i] <- 0
  }
}

# Now feed into the R2x Type and double check data quality of transition
for (i in 1:length(rx$nrdd_project_id)) {
  if (rx$adopter.comp[i] == "Adopter Exists" & rx$rx.qc.stat[i] == "No Transition"){
    rx$rx.qc[i] <- 0
  } else{}
}

proj.info.qc <- merge(proj.info.qc,rx[, c("nrdd_project_id","rx.qc.stat", "adopter.comp")], by = "nrdd_project_id", all.x = TRUE)
missing_data <- merge(missing_data,rx[, c("nrdd_project_id","rx.qc", "exp_trans", "adopter.qc")], by = "nrdd_project_id", all.x = TRUE)

############################
##RL and RL tracking table##
################################################################################

#load project info table
htm_tbl <- read_html("QueryBuilder_RL_Progress_06_07_23_All.HTM") #via HTML
rldata <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
rldata <- rldata %>% clean_names()

# If an Admin pulls the data and it includes non-approved data, 
# RL table must be filtered to only include Approved data.
rldata <- filter(rldata, nrdd_project_id %in% info$nrdd_project_id)

newdata <- rldata[ -c(2:11)]
newdata[newdata == ""] <- "NULL"
newdata[is.na(newdata)] <- "NULL"

#RL Expected Dates
#empty expected date fields
vector <- NA
rx <- newdata[c(3:38)]
for (i in 1: (as.numeric(length(newdata$nrdd_project_id))))
{
  vector[i] <- sum(rx[i,]== "NULL")
}
newdata$empty.qc <- ifelse(vector == 36, "Error: all date fields empty", "fine")

#compare expected to other RLfields
expected <- c(3,7,11,15,19,23,27,31,35)
active <- c(4,8,12,16,20,24,28,32,36)
complete <- c(5,9,13,17,21,25,29,33,37)
cancel <- c(6,10,14,18,22,26,30,34,38)
e.a.df <- data.frame(nrdd_project_id = newdata$nrdd_project_id)
e.c.df <- data.frame(nrdd_project_id = newdata$nrdd_project_id)
e.can.df <- data.frame(nrdd_project_id = newdata$nrdd_project_id)
for (i in 1:9)
{
  # If there is an active, completed, or cancel date, 
  # then there should be an expected date. 
  # Otherwise, assign error message.
  e.a <- ifelse(newdata[,expected[i]] == "NULL",
                ifelse(newdata[,active[i]] != "NULL", "expected empty, actual filled", "fine"),"fine")
  e.a.df <- cbind(e.a.df, e.a)
  e.c <- ifelse(newdata[,expected[i]] == "NULL",
                ifelse(newdata[,complete[i]] != "NULL", "expected empty, complete filled", "fine"),"fine")
  e.c.df <- cbind(e.c.df, e.c)
  e.can <- ifelse(newdata[,expected[i]] == "NULL",
                ifelse(newdata[,cancel[i]] != "NULL", "expected empty, cancel filled", "fine"),"fine")
  e.can.df <- cbind(e.can.df, e.can)
}
colnames(e.a.df) <- c("nrdd_project_id",1:9)
vector <- NA
rx <- e.a.df[c(2:10)]
for (i in 1: (as.numeric(length(e.a.df$nrdd_project_id))))
{
  vector[i] <- sum(rx[i,]== "fine")
}
newdata$expected.qc <- ifelse(newdata$empty.qc != "fine", newdata$empty.qc, 
                      ifelse(vector != 9, "expected date empty, active filled", "fine"))

colnames(e.c.df) <- c("nrdd_project_id",1:9) 
vector <- NA
rx <- e.c.df[c(2:10)]
for (i in 1: (as.numeric(length(newdata$nrdd_project_id))))
{
  vector[i] <- sum(rx[i,]== "fine")
}
newdata$expected.qc <- ifelse(newdata$expected.qc != "fine", newdata$expected.qc, 
                            ifelse(vector != 9, "expected date empty, completed filled", "fine"))

colnames(e.can.df) <- c("nrdd_project_id",1:9) 
vector <- NA
rx <- e.can.df[c(2:10)]
for (i in 1: (as.numeric(length(newdata$nrdd_project_id))))
{
  vector[i] <- sum(rx[i,]== "fine")
}
newdata$expected.qc <- ifelse(newdata$expected.qc != "fine", newdata$expected.qc, 
                            ifelse(vector != 9, "expected empty, cancelled filled", "fine"))

#RL active Dates
merge.df <- data.frame(nrdd_project_id = info$nrdd_project_id, current_project_status = info$current_project_status, current_rl = info$current_rl)
data <- merge(newdata,merge.df,by="nrdd_project_id", all.x = TRUE)
data <- data[!duplicated(data$nrdd_project_id), ]
data[data == ""] <- "NULL"
data[is.na(data)] <- "NULL"

#RL active vs complete
e.a.df <- data.frame(nrdd_project_id = data$nrdd_project_id)
for (i in 1:9)
{
  e.a <- ifelse(data[,active[i]] == "NULL",
                ifelse(data[,complete[i]] != "NULL", "active date empty, complete date filled", "fine"),"fine")
  e.a.df <- cbind(e.a.df, e.a)
}
colnames(e.a.df) <- c("nrdd_project_id",1:9)
vector <- NA
rx <- e.a.df[c(2:10)]
for (i in 1: (as.numeric(length(e.a.df$nrdd_project_id))))
{
  vector[i] <- sum(rx[i,]== "fine")
}
data$active.qc <- ifelse(data$empty.qc != "fine", data$empty.qc,
                         ifelse(vector != 9, "active date empty, complete date filled", "fine"))

#RL active vs status
e.a.df <- data.frame(nrdd_project_id = data$nrdd_project_id, current_project_status = data$current_project_status, current_rl = data$current_rl)
count <- 1
for (i in 1:9)
{
  e.a <- ifelse(data[,active[i]] == "NULL", "NULL", count)
  e.a.df <- cbind(e.a.df, e.a)
  count <- count + 1
}
colnames(e.a.df) <- c("nrdd_project_id", "current_project_status", "current_rl",1:9) 
vector <- NA
max.num <- NA
min.num <- NA
rx <- e.a.df[c(4:12)]
rx[rx == "NULL"] <- 0
for (i in 1: (as.numeric(length(e.a.df$nrdd_project_id))))
{
  vector[i] <- sum(as.numeric(rx[i,]))
  max.num[i] <- max(as.numeric(rx[i,]))
  min.num[i] <- ifelse(max.num[i] == 0, 0, (unique(sort(as.numeric(rx[i,])))[2]))
}
e.a.df$filled <- ifelse(vector != 0, "fine", "NULL")
e.a.df$min.num <- min.num
e.a.df$max.num <- max.num
for (i in 1: (as.numeric(length(e.a.df$nrdd_project_id))))
{
  e.a.df$status[i] <- ifelse(between(as.numeric(e.a.df$current_rl[i]),e.a.df$min.num[i], e.a.df$max.num[i]) == "TRUE", "fine", "current RL don't match")  
}
e.a.df$curr_status <- ifelse(e.a.df$current_project_status == "Planned",
                        ifelse(e.a.df$filled == "NULL", "fine", "Active date don't match project status"),
                        ifelse(e.a.df$filled == "NULL", "Active date don't match project status", "fine"))
e.c.df <- e.a.df[c(1,16,17)]
data <- merge(data,e.c.df,by="nrdd_project_id", all.x = TRUE)
data$active.qc <- ifelse(data$active.qc != "fine", data$active.qc, data$curr_status)
data$active.qc <- ifelse(data$active.qc != "fine", data$active.qc, data$status)
merge.df <- data[c(1,43)]
newdata <- merge(newdata,merge.df,by="nrdd_project_id", all.x = TRUE)


#RL Complete Dates
merge.df <- data.frame(nrdd_project_id = info$nrdd_project_id, current_project_status = info$current_project_status, current_rl = info$current_rl)
data <- merge(newdata,merge.df,by="nrdd_project_id", all.x = TRUE)
data <- data[!duplicated(data$nrdd_project_id), ]
data[data == ""] <- "NULL"
data[is.na(data)] <- "NULL"

#RL complete vs multiple actives
vector <- 0
com.vector <- 0
status <- NA
rx <- data[c(active)]
complete.df <- data[c(complete)]
for (i in 1: (as.numeric(length(data$nrdd_project_id))))
{
  vector[i] <- 9 - sum(rx[i,]== "NULL")
  com.vector[i] <- 9 - sum(complete.df[i,]== "NULL")
  status[i] <- ifelse(vector[i] <= com.vector[i] + 1, "fine", "Complete Date empty but multiple Actual Date")
}
data$complete.qc <- ifelse(data$empty.qc != "fine", data$empty.qc, status)

#RL complete vs status
complete.df <- data[c(1,5,9,13,17,21,25,29,33,37,42)]
rx <- complete.df[c(2:10)]
vector <- 0
for (i in 1: (as.numeric(length(complete.df$nrdd_project_id))))
{
  vector[i] <- sum(rx[i,]== "NULL")
  complete.df$status[i] <- ifelse(complete.df$current_project_status[i] == "Completed", 
                                 ifelse(vector[i] == 9, "Complete Date empty but Status is Complete", "fine"), "fine")
}
data$complete.qc <- ifelse(data$complete.qc != "fine", data$complete.qc, complete.df$status)
merge.df <- data[c(1,44)]
newdata <- merge(newdata,merge.df,by="nrdd_project_id", all.x = TRUE)

#merge back
merge.df <- newdata[c(1,40,41,42)]
info <- merge(info,merge.df,by="nrdd_project_id", all.x = TRUE)
info[is.na(info)] <- "Need data in RL table"

info <- info[order(info$nrdd_project_id),]
proj.info.qc <- proj.info.qc[order(proj.info.qc$nrdd_project_id),]
missing_data <- missing_data[order(missing_data$nrdd_project_id),]

#RL table
for (i in 1: (as.numeric(length(info$nrdd_project_id))))
{
  missing_data$rl_expected_dates[i] <- ifelse(info$expected.qc[i] == "fine", 1, 0)
  missing_data$rl_active_dates[i] <- ifelse(info$active.qc[i] == "fine", 1, 0)
  missing_data$rl_completed_dates[i] <- ifelse(info$complete.qc[i] == "fine" , 1, 0)
}
# Select the following 9 data fields from the Info table 
# NRDD ID, LO, Office, Division, last_update_yr, updated_by, 
# expected.qc, active.qc, complete.qc
rl.table.qc <- info[c(1,4,5,6,44,43,45,46,47)]

# Select the following 4 data fields:
# NRDD ID, Office, Division, Updated_By
merge.df <- info[c(1,5,6,43)]
proj.info.qc <- merge(proj.info.qc,merge.df,by="nrdd_project_id", all.x = TRUE)

# Select the following 15 data fields:
# NRDD Project ID, LO, Office, Division, last_update_yr, updated_by
# Project Des, Benefits, Outcome,
# Actual_start, Actual_end, current_project_stat,
# project_planned_duration, r2x_type
proj.info.qc <- proj.info.qc[c(1,2,12,13,3,14,4:11)]


#####################
## Resource tables ##
###############################################################################
#load actual resource data
actual <- read.csv("DW_Direct_Funds_Resources (TableauSQL)_DW_Direct_Funds_Resources.CSV", stringsAsFactors = FALSE, sep=",", header = TRUE, strip.white = TRUE)
actual <- actual %>% clean_names()
actual[actual == ""] <- NA
actual[is.na(actual)] <- "NULL"
actual <- filter(actual, nrdd_project_status =="Approved")

#actual and planned funds field completion
for (i in 1: (as.numeric(length(actual$nrdd_project_id))))
{
  # If the Actual Funds are NOT NULL, test for 0/1, else fine
  # If the Actual Funds are NULL, 
  # and Planned Funds are NOT Null, Did not recieve expected funds
  # and Planned Funds are NULL, some years with no entry
  actual$actualstatus[i] <- ifelse(actual$actual_funds[i] != "NULL", 
                                   ifelse(actual$actual_funds[i] == 0, "Entered $0 or $1", 
                                          ifelse(actual$actual_funds[i] == 1, "Entered $0 or $1", "fine")),
                                   ifelse(actual$planned_funds[i] != "NULL", "Did not receive expected funds?", "some years with no entry"))
 
  # If the Planned Funds are NOT NULL, test for 0/1, else fine
  # If the Planned Funds are NULL, 
  # and Actual Funds are NOT NULL, Received unexpected funds
  # and Actual Funds are NULL, some years with no entry
  actual$plannedstatus[i] <- ifelse(actual$planned_funds[i] != "NULL", 
                                    ifelse(actual$planned_funds[i] == 0, "Entered $0 or $1",
                                           ifelse(actual$planned_funds[i] == 1, "Entered $0 or $1", "fine")),
                                    ifelse(actual$actual_funds[i] != "NULL", "Received unexpected funds?", "some years with no entry"))
}

# For each unique project, find the issue with the ACTUAL resource info
list <- unique(actual$nrdd_project_id)
proj <- NA

for (i in 1: (as.numeric(length(list))))
{
  temp <- filter(actual, nrdd_project_id == list[i])
  if(as.numeric(length(unique(temp$actualstatus))) == 1) {
    # If there is only one unique status for the project,
    # assign the stat as such for that project
    proj[i] <- unique(temp$actualstatus)
  }else {
    # otherwise assign NA for all fine status
    temp[temp == "fine"] <- NA
    vector <- unique(na.omit(temp$actualstatus))
    # get array of status where it is not fine
    if(as.numeric(length(vector)) == 1) {
      proj[i] <- vector
    }else {
      proj[i] <- "multiple issues"
    }
  }
}
actual_fund <- data.frame(nrdd_project_id = list, actual.qc = proj)
#planned by project
list <- unique(actual$nrdd_project_id)
proj <- NA
for (i in 1: (as.numeric(length(list))))
{
  temp <- filter(actual, nrdd_project_id == list[i])
  if(as.numeric(length(unique(temp$plannedstatus))) == 1) {
    proj[i] <- unique(temp$plannedstatus)
  }else {
    temp[temp == "fine"] <- NA
    vector <- unique(na.omit(temp$plannedstatus))
    if(as.numeric(length(vector)) == 1) {
      proj[i] <- vector
    }else {
      proj[i] <- "multiple issues"
    }
  }
}
planned_fund <- data.frame(nrdd_project_id = list, planned.qc = proj)
fund.qc <- merge(actual_fund,planned_fund,by="nrdd_project_id", all.x = TRUE)
resource.qc <- merge(info, fund.qc, by="nrdd_project_id", all.x = TRUE)

# Select the following 8 data fields:
# NRDD_ID, LO, Office, Division, last_update_yr, updated_by,
# actual_funds, planned_funds
resource.qc <- resource.qc[c(1,4,5,6,47,46,51,52)]
resource.qc[is.na(resource.qc)] <- "No Resource info for this Project"

#load leveraged resource data
leverage <- read.csv("DW_Leveraged_Resources (TableauSQL)_DW_Leveraged_Resources.CSV", stringsAsFactors = FALSE, sep=",", header = TRUE, strip.white = TRUE)
leverage <- leverage %>% clean_names()
leverage[leverage == ""] <- NA
leverage[is.na(leverage)] <- "NULL"
leverage <- filter(leverage, nrdd_project_status =="Approved")

#leverage.qc <- data.frame(nrdd_project_id = unique(leverage$nrdd_project_id))
for (i in 1: (as.numeric(length(leverage$nrdd_project_id))))
{
  vector <- sum(leverage[i,7:11]== "NULL")
  if(vector == 5){
    leverage$leverage.qc[i] <- "No Resources Info for this project"
  } else {
    leverage$leverage.qc[i] <- "fine"
  }
}
leverage.qc <- leverage[,c("nrdd_project_id", "leverage.qc")]
proj2 <- NA

list <- unique(leverage.qc$nrdd_project_id)
for(i in 1: (as.numeric(length(list)))) 
{ 
  temp <- filter(leverage.qc, nrdd_project_id == list[i])
  if("fine" %in% temp) {
    proj2[i] <- "fine"
  } else {
    proj2[i] <- temp$leverage.qc[1]
  }
}
leverage.qc <- data.frame(nrdd_project_id = list, leverage.qc = proj2)
resource.qc <- merge(resource.qc, leverage.qc, by="nrdd_project_id", all.x = TRUE)
resource.qc[is.na(resource.qc)] <- "No Resources Info for this project"

#load other leverage resource data
other <- read.csv("DW_Other_Leveraged_Resources (TableauSQL)_DW_Other_Leveraged_Resources.CSV", stringsAsFactors = FALSE, sep=",", header = TRUE, strip.white = TRUE)
other <- other %>% clean_names()
other[other == ""] <- NA
other[is.na(other)] <- "NULL"
other <- filter(other, nrdd_project_status =="Approved")
other.qc <- data.frame(nrdd_project_id = unique(other$nrdd_project_id), other.qc = "fine")
resource.qc <- merge(resource.qc, other.qc, by="nrdd_project_id", all.x = TRUE)
resource.qc[is.na(resource.qc)] <- "No Resource info"

temp <- resource.qc[c(1,7:10)]
temp$actual.qc <- ifelse(temp$actual.qc == "fine", 1, 0)
temp$planned.qc <- ifelse(temp$planned.qc == "fine", 1, 0)
temp$leverage.qc <- ifelse(temp$leverage.qc == "fine", 1, 0)
temp$other.qc <- ifelse(temp$other.qc == "fine", 1, 0)
temp <- temp[order(temp$nrdd_project_id),]
missing_data <- missing_data[order(missing_data$nrdd_project_id),]
missing_data <- cbind(missing_data, temp[c(2:5)])

#####################
## Partners tables ##
###############################################################################
#load NOAA Partners, External Partners, and Stakeholders  data
htm_tbl <- read_html("QueryBuilder_NOAA_Partners_06_07_23_All.HTM") #via HTML
noaa_partner <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
noaa_partner <- noaa_partner %>% clean_names()

htm_tbl <- read_html("QueryBuilder_External_Partners_06_07_23_All.HTM") #via HTML
ext_partner <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
ext_partner <- ext_partner %>% clean_names()

htm_tbl <- read_html("QueryBuilder_StakeHolders_06_07_23_All.HTM") #via HTML
stakeh <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
stakeh <- stakeh %>% clean_names()

# If an Admin pulls the data and it includes non-approved data, 
#  table must be filtered to only include Approved data.
noaa_partner <- filter(noaa_partner, nrdd_project_id %in% info$nrdd_project_id)
ext_partner <- filter(ext_partner, nrdd_project_id %in% info$nrdd_project_id)
stakeh <- filter(stakeh, nrdd_project_id %in% info$nrdd_project_id)

# Get unique project IDs only.
noaa_partner <- noaa_partner %>% distinct(nrdd_project_id, .keep_all = TRUE)
ext_partner <- ext_partner %>% distinct(nrdd_project_id, .keep_all = TRUE)
stakeh <- stakeh %>% distinct(nrdd_project_id, .keep_all = TRUE)

noaa_partner$noaa_partner_qc <- "fine"
ext_partner$ext_partner_qc <- "fine"
stakeh$stakeh_qc <- "fine"

partner.qc <- info[,c(1,4:6, 47)]
partner.qc <- merge(partner.qc, noaa_partner[,c(1,16)], by = "nrdd_project_id", all.x = TRUE)
partner.qc <- merge(partner.qc, ext_partner[,c(1,17)], by="nrdd_project_id", all.x = TRUE)
partner.qc <- merge(partner.qc, stakeh[,c(1,16)], by="nrdd_project_id", all.x = TRUE)

partner.qc[partner.qc == ""] <- NA
partner.qc[is.na(partner.qc)] <- "NULL"

temp <- partner.qc
temp$noaa_partner_qc <- ifelse(temp$noaa_partner_qc == "fine", 1, 0)
temp$ext_partner_qc <- ifelse(temp$ext_partner_qc == "fine", 1, 0)
temp$stakeh_qc <- ifelse(temp$stakeh_qc == "fine", 1, 0)

missing_data <- merge(missing_data,temp[, c("nrdd_project_id", "noaa_partner_qc", "ext_partner_qc", "stakeh_qc")],
                      by="nrdd_project_id", all.x = TRUE)

#####################
## Milestones and Deliverables tables ##
###############################################################################
#load NOAA Milestones and Deliverables data
# htm_tbl <- read_html(file.choose()) #via HTML
# milestones <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
# milestones <- milestones %>% clean_names()
# milestones <- filter(milestones, nrdd_project_id %in% info$nrdd_project_id)
# 
# 
# milestones <- milestones %>% distinct(nrdd_project_id, .keep_all = TRUE)
# milestones$milestone_qc <- "fine"
# milestone.qc <- info[,c(1,4:6,44)]


## Get Percent Quality of ALL Data Fields -----------------------------
missing_data[is.na(missing_data)] <- 0
missing_data_qc <- missing_data[,-(1:4)]
header <- c("sum", colnames(missing_data_qc))

info.null <- 0
info.null[1] <- length(missing_data$nrdd_project_id)
#info.null[2:length(header)] <- as.integer(colSums(missing_data_qc)/info.null[1]*100)
info.null[2:length(header)] <- round(colMeans(missing_data_qc), 4)*100
missing_data_list <- data.frame(field = header, overall = info.null)

## The next few lines of code does not work in Looker/Google Data Studio
## We lose the total count of projects/records per LO/office/year
## missing_group <- missing_data[, -1] %>%
##   group_by(LO, Office, yr_updated) %>%
##   summarise_all("mean") %>%
##   as.data.frame()
## 
## missing_group[,-c(1:3)] <- round(missing_group[,-c(1:3)], 4)*100

## Save Files to Directory ---------------------------------------------------
# Write the name of the file after choosing file location

# write.csv(proj.info.qc, file.choose()) #"proj_info_qc.csv"
# write.csv(rl.table.qc, file.choose()) #"rl_table_qc.csv"
# write.csv(resource.qc, file.choose()) #"resources_qc.csv"
write.csv(missing_data, file.choose()) #"data_4_qcdashboard.csv"
