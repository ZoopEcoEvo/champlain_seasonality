# Seasonality of Lake Champlain Copepod Thermal Limits	

Matthew Sasaki<sup>1</sup>

1. University of Vermont, Department of Biology	

This project examines patterns in thermal limit variation across and within the species of calanoid copepods found in Lake Champlain. Animals were collected at approximately weekly intervals from a site in Burlington Vermont.    

## Directory Structure 
The root directory contains the README and Licensing files, along with a .Rproj file and four sub-directories: Data, Manuscript, Output, and Scripts.  

-   `Manuscript/` contains the R Markdown file, templates, and bibliography that will be used to produce the manuscript draft, should the project result in a publication. 

-   `Output/` contains the various products of the project (processed data, figures, knit reports, etc.). Note, the `Reports/` sub-directory contains the R Markdown file used to generate the figures used in the manuscript.  

-   `Raw_Data/` contains the raw data used in this analysis.  

-   `Scripts/` contains two R scripts. 
    -   `01_Data_analysis.R` is used to process the raw data. The primary component is the conversion from timepoint measurements of when each individual reached its CTmax to a measurement in °C using the continuous temperature record from the experiment. 
    -   `02_make_report.R` is use to control the project workflow. Through this script, you can choose to run the process the data, make the figures, or knit the manuscript. This script should be used rather than running isolated fragments individually. 


## Data Description 	

The `Raw_Data/` directory contains two sub-directories for the different component data files.

The `pheno_obs` directory contains the observed phenotypic data from each CTmax replicate. Each .csv file represents the observations from one experiment. File names include the date the experiment was performed in YYYY_MM_DD format. Each file contains the following columns:
    -   *collection_date* - The date copepods were collected from Lake Champlain. 	  	
    -   *collection_temp*	- The temperature measured at the time of collection in °C. Temperature was measured in the cod end of the plankton net using a manual thermometer.	
    -   *experiment* - An internal variable. Indicates which of several co-occurring projects the experiment was part of. 		   
    -   *replicate* - Indicates when more than one experiment was run per day. 		   
    -   *species* - The individual species, sex, and/or stage ID observed.		   
    -   *tube* - Identifies which tube the individual was held in during the experiment. 		   
    -   *ctmax_minute* - The minute component of the time the individual was observed to reach its CTmax. 		   
    -   *ctmax_second* - The second component of the time the individual was observed to reach its CTmax. 		   
    -   *fecundity* - The number of eggs carried by the individual (if present). 		   
    -   *size* - The prosome length of the individual (in mm). 		   

The `temp_data` directory contains the continuous temperature data recorded during each CTmax assay. Each .csv file represents the recordings from one experiment. File names include the date the experiment was performed in YYYY_MM_DD format. Each file contains the following columns:
    -   *Date* - The date recorded by the temperature logger. Note, these dates may not reflect the actual experimental dates - all information about the date the experiment was performed should be taken from the file name.    	
    -   *Time*	- The time each temperature measurement was recorded. Again, the absolute time may not be correct, but this column is used to follow the progression of the CTmax assay.	
    -   *Temp1/Temp2/Temp3* - The temperatures (in °C) recorded by each of the three temperature sensors, recorded in their own columns. 	
    
    
## Workflow

The workflow is operated via the 02_Make_report.R script in the Scripts directory. It is not recommended that you run analyses or knit documents from the files themselves as the file paths are internally set and may not be correct otherwise. At the top of this script, you are able to indicate whether:

1. The raw data should be processed to calculate thermal limits from the observed CTmax time point measurements and the continuous temperature record.  

2. The summary file (located in the Output/Reports directory) should be knit. This markdown file will generate the figures used in the manuscript, as well as an HTML and a GitHub flavored markdown document.

3. The manuscript file (located in the Manuscripts directory) should be knit. This markdown file will produce formatted PDF and word document versions of the manuscript. 


## Versioning   

-To be filled in after project completed- 

## Funding

This study was funded by an NSF postdoctoral fellowship awarded to MCS (OCE-2205848).
