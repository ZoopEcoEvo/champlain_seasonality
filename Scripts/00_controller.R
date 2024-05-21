# Load in required packages
library(rmarkdown)
library(tidyverse)
library(ggridges)
library(readxl)
library(ggpubr)
library(patchwork)
library(dataRetrieval)
library(lubridate)
library(slider)
library(english)
library(lme4)
library(mgcv)
library(dlnm)

#Determine which scripts should be run
process_all_data = F #Runs data analysis 
process_sequences = F #Analyzes the COI sequence data
make_report = T #Runs project summary
predict_vuln = F #Runs the thermal stress hindcast portion: This can be slow! 
knit_manuscript = F #Compiles manuscript draft

############################
### Read in the RAW data ###
############################

source(file = "Scripts/01_data_processing.R")
source(file = "Scripts/02_ab1_to_fasta.R")

##################################
### Read in the PROCESSED data ###
##################################

full_data = read.csv(file = "Output/Data/full_data.csv") %>%  
  drop_na(ctmax) %>% 
  mutate("sp_name_sub" = str_replace_all(species, pattern = "_", replacement = " "),
         sp_name_sub = str_to_sentence(sp_name_sub), 
         "sp_name" = word(sp_name_sub, start = 1, end = 2),
         "sex" = case_when( # creates a new column called "sex" that is filled with different values when...
           word(sp_name_sub, start = 3, end = 3) == "male" ~ "male", #... the third word in sp_name_sub is 'male'
           word(sp_name_sub, start = 3, end = 3) == "juvenile" ~ "juvenile", #... or the third word in sp_name_sub is 'juvenile'
           TRUE ~ "female")) %>% # In all other cases, 'female' is used
  filter(sp_name != "Leptodora kindti") %>% 
  mutate(collection_date = as_date(collection_date))

temp_data = read.csv(file = "Output/Data/champlain_temps.csv") %>% 
  mutate(date = lubridate::as_date(date))

hind_temp_data = read.csv(file = "Output/Data/hindcast_temps.csv") %>%  
  mutate(date = as_date(date))

sic_dnabin = adegenet::fasta2DNAbin(file = "Output/Sequences/Sanger_contigs_alignment.fa")

if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################


if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
                                                                  #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
