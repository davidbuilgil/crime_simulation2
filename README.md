**Data and code for a project on bias in crime statistics using simulated data.**

Steps to replicate results are split into two sections. First, guidance is provided on how to download the necessary data, all of which is openly available. Second, we describe how to run the R scripts contained in this repository.


_Data download guide_

All data used in this project is either available under [Open Government Licence](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) or simulated within the R environment. The following steps provide guidance on how to accesss and download the resident population data from the census used in the scripts.
  
USUAL RESIDENT POPULATION (to access area population size, and number of males per Output Area).  
Download from [NOMIS website](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=144). 
ON THE LEFT PANEL: Click on ‘Geography’  
FROM ‘SELECT USING LIST’ OPTIONS: Click on ‘Output area’ and select ‘Some’  
Choose ‘Manchester’ within the option ‘List areas within’, and click on ‘tick all’  
ON THE LEFT PANEL: Click on ‘Variable’ and tick the options ‘All usual residents’, ‘Males’, and ‘Females’  
ON THE LEFT PANEL: Click on ‘Download data’ and then ‘Download data for Excel 2007 (.xlsx)’  
  
AGE STRUCTURE (to access residents aggregated by age per Output Area).  
Download from [NOMIS website](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=145).  
ON THE LEFT PANEL: Click on ‘Geography’  
FROM SELECT USING LIST: Click on ‘Output area’ and select ‘Some’  
Choose ‘Manchester’ within the option ‘List areas within’, and click on ‘tick all’  
ON THE LEFT PANEL: Click on ‘Age’ and tick the options ‘All usual residents’, all age groups and ‘Mean’  
ON THE LEFT PANEL: Click on ‘Download data’ and then ‘Download data for Excel 2007 (.xlsx)’  
  
ETHNIC GROUP (to access number of whites per Output Area).
Download from [NOMIS website](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=608).  
ON THE LEFT PANEL: Click on ‘Geography’  
FROM SELECT USING LIST: Click on ‘Output area’ and select ‘Some’  
Choose ‘Manchester’ within the option ‘List areas within’, and click on ‘tick all’  
ON THE LEFT PANEL: Click on ‘Ethnic Group’ and tick the options ‘All usual residents’ and ‘White’  
ON THE LEFT PANEL: Click on ‘Download data’ and then ‘Download data for Excel 2007 (.xlsx)’  
  
ECONOMIC ACTIVITY (to access number of residents without employment by Output Area).  
Download from [NOMIS website](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=556).    
ON THE LEFT PANEL: Click on ‘Geography’  
FROM SELECT USING LIST: Click on ‘Output area’ and select ‘Some’  
Choose ‘Manchester’ within the option ‘List areas within’, and click on ‘tick all’  
ON THE LEFT PANEL: Click on ‘Economic Activity’ and tick the options ‘All categories: Economic activity’, ‘Unemployed’, ‘Full-time student’ and ‘Economically inactive: Total’  
ON THE LEFT PANEL: Click on ‘Download data’ and then ‘Download data for Excel 2007 (.xlsx)’  
  
HIGHEST LEVEL OF QUALIFICATION (to access number of residents with a higher education certificate by Output Area).  
Download from [NOMIS website](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=554).  
ON THE LEFT PANEL: Click on ‘Geography’  
FROM SELECT USING LIST: Click on ‘Output area’ and select ‘Some’  
Choose ‘Manchester’ within the option ‘List areas within’, and click on ‘tick all’  
ON THE LEFT PANEL: Click on ‘Qualification’ and tick the options ‘All categories: Highest level of qualification’ and ‘Level 4 qualifications and above’  
ON THE LEFT PANEL: Click on ‘Download data’ and then ‘Download data for Excel 2007 (.xlsx)’ 

COUNTRY OF BIRTH (to access residents aggregated by age per Output Area).  
Download from [NOMIS website](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=611).  
ON THE LEFT PANEL: Click on ‘Geography’  
FROM SELECT USING LIST: Click on ‘Output area’ and select ‘Some’  
Choose ‘Manchester’ within the option ‘List areas within’, and click on ‘tick all’  
ON THE LEFT PANEL: Click on ‘Country of birth’ and tick the options ‘All usual residents’ and ‘United Kingdom’  
ON THE LEFT PANEL: Click on ‘Download data’ and then ‘Download data for Excel 2007 (.xlsx)’  

MARRIAGE STATUS (to access residents aggregated by age per Output Area).  
Download from [NOMIS website](https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=603).  
ON THE LEFT PANEL: Click on ‘Geography’  
FROM SELECT USING LIST: Click on ‘Output area’ and select ‘Some’  
Choose ‘Manchester’ within the option ‘List areas within’, and click on ‘tick all’  
ON THE LEFT PANEL: Click on ‘Marital status’ and tick the options ‘All usual residents’, ‘Married’ and ‘In a registered same-sex civil partnership’  
ON THE LEFT PANEL: Click on ‘Download data’ and then ‘Download data for Excel 2007 (.xlsx)’  

Police recorded crime data for Greater Manchester Police can be obtained from the [open data portal](https://data.police.uk/data/). The time period is selected via the dropdown menus, and police force selected using tickboxes. Files are downloaded month-by-month in .csv format.

Index of Multiple Deprivation 2010 data are downlaoded from the file 'English indices of deprivation 2010: all domains, all sub domains and supplementary indices' in [GOV.UK](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2010).

_Script run guide_.

All scripts should be ran from within the 'crime_simulation.Rproj' R project file.

1. R script files named 'replicate_*' (e.g. replicate_age.R') are used to wrangle
   census data into an appropriate format for usage in the main script. These scripts
   should be run first, taking note of the data guidelines outlined above.
2. R script file named 'data_convert_preperation.R' is required mainly to reduce the size
   of data files detailed in _data description_. When downloaded using the steps provided,
   the data is too large in its raw form. Use this script to load, wrangle, and re-save the data.
3. R script file named 'simulation_analysis.R' is the main script to replicate the study. It can only
   be ran once data has been processed in steps 1 and 2. It generates and saves the visuals. Statistics
   reported in the paper are printed to the Console.
