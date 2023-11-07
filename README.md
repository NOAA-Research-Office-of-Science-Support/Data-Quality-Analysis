# Data-Quality-Analysis

This read me file serves to explain the business rules that are used to assess the data quality of the NOAA Research and Development Database (NRDD) data. 

## Method
### Data Quality Assessment Rules
Data quality can be indicated by how often the field is completed in the correct manner or the extent to which it conflicts with the information provided by another data field. Some data fields are not evaluated for their quality but their completeness. Due to the mandatory or optional nature of the NRDD data fields, the completeness of the entries for those fields will differ. For example, every NRDD project should have a Title but not every project will have a Transition Plan. Therefore, it is not guaranteed that every NRDD project will have an entry for every single data field and the business rules will vary. 

The code for assessing the data quality is written in a single .R file using RStudio. The code produces a matrix of 1s and 0s in which each column of the matrix represents a data field and each row is attributed to the individual NRDD project, the year the project was last updated, the LO and FMC that funds the project . A value of 1 represents if the NRDD project met the business rule conditions for the NRDD data field, indicating that it is of quality. A value of 0 indicates that the entry failed to meet the condition. Taking the average of each column will provide the overall average, in percent, for the NRDD data quality. 

There are several data fields that would require a data quality assessment beyond the conditional if-else checks. Although the database system does automatically check to see if the data enterer has inputted information for the Strategic Plan linkages, or the Milestones and Deliverables, it does not ensure that the information provided is of quality. For example, it has been found that although everyone maps their project to a NOAA strategy, they are not always mapped correctly. Hence, those data fields require a qualitative analysis to determine their data quality and are not represented here. The following sections describe the quantitative method to calculate the NRDD’s data quality.

Project LO, Office, Division [Required]
The Accountable Organization is the primary NOAA funding office of the project (i.e., the office that provides the most funding or resources for the project). 

Condition: 
If the project has an assigned LO, assign 1, else assign 0.
If the project has an assigned Office, assign 1, else assign 0.
If the project has an assigned Division, assign 1, else assign 0.

Project Title [Required]
The Project Title is the name of the project and must be unique within the NRDD Database. The database automatically checks for duplication of project names. 

Condition: If the project has a Project Title, assign 1, else assign 0.

Project Description [Required]
The Project Description is an overview of the project: its context, purpose, importance, and goals. Many times the description can be parsed from existing proposals or abstracts.

Condition: If the project has a Project Description, assign 1, else assign 0.

Project Benefits and Outcome [Required]
The Benefits field captures how the project will benefit society and the world.  The Outcomes field describes the scientific contribution or advancement that the project will enable. 
Some users, to avoid having to share the benefits and outcome and pass the error checks enabled by the database, will copy and paste the Project Description into the Benefits and Outcomes field. 

Condition: If the Project Benefits and Outcome are unique to each other and to the Project Description, assign 1, else 0.

Current Project Status [Required]
The Current Project Status indicates the current stage at which the project is in. For example, if a project is ‘In Progress’ that means it has begun. A project in ‘Completed’ status has had all of their R&D Activities completed. This data field requires an entry and must be consistent with the NRDD date fields.

Condition: 
If Current Project Status is missing, assign zero.
If the Current Project Status is consistent with the NRDD Date Fields, assign 1, else 0.
Unfunded: No actual start or end date
Planned: No actual start or end date
In Progress: Mandatory actual start date, No actual end date
Completed: Mandatory actual start date, Mandatory actual end date
Transitioned: Mandatory actual start date, Mandatory actual end date
Pending Transition(s): Mandatory actual start date, Mandatory actual end date
Pending Deliverable(s): Mandatory actual start date, Mandatory actual end date
Pending Transition(s) & Deliverable(s):  Mandatory actual start date, Mandatory actual end date
Delayed: Optional actual start date, No actual end date.
On hold: Optional actual start date, No actual end date.
Canceled: Optional actual start date, Mandatory actual end date.

Planned Project Start, Planned Project End, Actual Project Start, Actual Project End [Required]
The Planned Start and End Dates are estimates of when the project is planned to start. The Actual Start and End dates are entered once the project has initiated or completed its R&D activities. These four date fields must also be consistent with the NRDD Current Project Status. 

Condition: 
If there is no Planned Project start date, assign zero, else 1.
If there is no Planned Project end date, assign zero, else 1.
If there is no Actual Start Date, then assign zero when the Current Project Status is In Progress, Completed, Transitioned, Pending Transitioned, Pending Deliverables, when Current Project Status is NULL, or when there is an Actual End Date. Else, assign 1.
If there is an Actual Start Date, then assign zero when Current Project Status is Planned or Unfunded. Else, assign 1.
If there is no Actual End Date, then assign zero when the Current Project Status is Completed, Transitioned, Pending Transitioned, Pending Deliverables, Canceled, or when the Current Project Status is also NULL. Else, assign 1.
If there is an Actual End Date Date, then assign zero when Current Project Status is Unfunded, Planned, In Progress, Delayed, or On Hold. Else, assign 1.

Alternate LO Project ID / Alternate Project ID Type [Optional]
The Alternate/LO Project ID is an optional field in which project owners may enter an alternate/custom project ID number to allow for cross-referencing NRDD IDs with other databases or data records used by the Accountable Office. 

Condition:
If there is an Alternate Project ID, assign 1, else 0.
If there is an Alternate Project ID Type, assign 1, else 0.

R2X and Transition Information [Optional]
Not all R&D Projects are expected to transition. Currently, the NRDD system asks the user if their project has a transition plan instead of asking if the project intends to transition. Some users select ‘No’ to having a transition plan, even if the project is an R2X project because they do not intend on developing a transition plan. Other users select Yes to having a transition plan, even if they do not have a transition plan so that they may be able to respond to the other NRDD Transition fields. Because the Transition Plan (y/n) field has discrepancies in its responses, the ‘R2X Type’ data field was used as a proxy to indicate whether the project is an R2X project or not. The Transition Plan (y/n) field was not used in the analysis. If the user skips the transition table information altogether, the entries are provided NULL values. Additionally, the contents of the R2X and Transition data fields should be consistent with each other. 

Condition for R2X Type: 
If there is no info (NULL values), assign 0. If the user answered yes/no, assign 1.
If there is an Adopter listed for the transition, but the R2X is no, assign 0. 
	Condition for Expected Transition Date:
If there is an R2X Type (indicated a transition) and there is an Expected Transition Date, assign 1, else 0.
If there is no R2X Type (indicating no transition) and there is no Expected Transition Date, assign 1, else 0.
	Condition for External or NOAA Adopter Field:
If the project intends to transition and there is an External or NOAA Adopter, assign 1, else 0.

Readiness Level (RL) and RL Tracking Table [Required]
Each project should have a single Current RL at any given time, and the Current RL selection should be adjusted whenever the project progresses to a new RL. The RL Progress table tracks the project’s planned and actual progression through the readiness levels using their planned and active dates. Every RL that the project intends to reach should have an Expected Date. The Current RL should have both Expected and Active Dates.

Condition for the Current RL:
If the project is assigned a Current RL, assign 1, else 0. 
	Conditions for RL Expected Date:
If the project has no RL Expected Date, assign 0
OR If there is an Active, Completed, or Canceled Date but no Expected Date, assign 0
Else 1.
	Conditions for RL Active Date:
If the Current RL is not between the minimum and maximum RL, assign 0
If the project has a Completed Date but no Active Date, assign 0
If the project is in Planned Status and there is an Active Date, assign 0
If the project is not in Planned Status and there is no Active Date, assign 0
Else 1
	Conditions for RL Completed Date:
If there are multiple Active Dates, and no Completed Dates for the progressed RLs, assign 0.
If the Current Project Status is Completed and there is no RL Completed Date, assign 0
Else 1
 
Resources Information [Required]
Although the NRDD is not a budget tracking tool, users are expected to provide estimations of their project resources to provide context for the size of a project. However, users have shared that it is difficult to track funding information when it comes from multiple sources (Cross, 2023). Consequently, users will provide inaccurate representations of their project’s budget or, alternatively, users will enter $1 or $0 to avoid the mandatory input response required by the NRDD website. 

Conditions for Direct Funds
Each project can have multiple years of funding. Projects pulled through a data ingestion will only show data entered for those years, otherwise no data will be visible.
If Direct Funds show $1 or $0, assign 0
If the project is expecting funds and there are none reported, assign 0
If the project does not have any Direct Funds info available, assign 0
Else 1
	Conditions for Leveraged Funds
Each project can have multiple years of leveraged resources. Not all projects are expected to have leveraged resources.
If the Leverage Resources are available, assign 1, else 0.
Other Leveraged Resources
If Other Leveraged Resources are provided, assign 1, else 0.

### Looker Studio
Looker Studio, formerly known as Google Data Studio, is a business intelligence tool that enables straightforward development of dashboards and provides visualization features. The R code that completes the data quality assessment of the NRDD data fields described in the previous section produces a matrix of 1s and 0s. Each column of the matrix is represented by an NRDD data field and each row represents a composite of the Line Office, FMC, and the year the project was last updated in the NRDD. This matrix was pulled into Looker Data Studio as a data source. Each column of the matrix was averaged to produce the average rate of completeness/quality and converted into a percent value. Looker Studio does not have advanced capabilities to manipulate data or metadata and requires the user to perform some manual data wrangling within the dashboard as well as prior to loading into the dashboard.

## Caveats and Limitations of Quantitative Analysis
Although it is useful to calculate the quality of data by comparing the inconsistencies of data entries to each other, there are limiting factors that prevent this type of standardized assessment from being performed on other NRDD data. A quantitative analysis was not performed on the Milestones, Deliverables, or Strategic Linkages. This is because users are required by the NRDD system to input this information anyways. However, the quality of these data are not impacted by their inconsistency to other data but by the content of the information provided and will require a qualitative analysis. The Milestones and Deliverables data fields are free-text fields. Hence, a user may simply write “TBA” to avoid entering an actual milestone. Additionally, some of the milestones listed in their timeline are actually known to be deliverables. A look into the Deliverables entries will show that users will select “Other” instead of selecting the correct dropdown selection. Given this information, anyone viewing the NRDD dashboard should understand that the percent values listed are on the liberal side. In actuality, the data quality may be much lower after evaluating the qualitative nature of the metadata. 
