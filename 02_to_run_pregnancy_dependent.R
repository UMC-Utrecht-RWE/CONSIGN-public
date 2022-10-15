#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#20/9/2022
#further development 5/10/22

#pregnancy-DEPENDENT scripts (waiting for pregnancy algorithm update)

# make sure you have the location, format and file name of your pregnancy data in params.R

# make sure you have covid_data.csv in the preselect_folder (if it is in another location, please copy and paste it to preselect_folder)

# make sure you have ALL_full_codelist.csv in projectFolder

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

# CHECK/ FILL IN YOUR PARAMETERS FOR PREGNANCY ALGORITHM FILE 

source("params.R")

source("99_path.R")

suppressMessages(source(paste0(pre_dir, "/packages.R")))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

source(paste0(pre_dir,"/CreateEntryExit.R"))

# adding variables to pregnancy data
# calculates trimester dates AND maternal age categories and adds these variables to original pregnancy data
source(paste0(pre_dir, "/trimester_create.R"))

# creates column age_group for matching
#applying exclusion criteria and creating pregnancy cohorts 
    #filters out red quality pregnancies
    #checks pregnancies have 12 months follow up from preg_start_date
    # creates subsets of pregnancy-having (historical and pandemic) and non-pregnancy-having women

source(paste0(pre_dir, "/pregnancy_filter.R"))

# calculates if death occurred within 28 days of covid date using DOD generated in CreateEntryExit
source(paste0(pre_dir,"/covid_death.R"))

# tests covid_dates against pregnancy dates
# if covid_date is during pregnancy, cov_trim shows which trimester the first covid_date was in

source(paste0(pre_dir, "/cov_trimester_function.R"))

source(paste0(pre_dir, "/trimester_covid.R"))

#sorts cohorts according to pregnancy and covid status
# deletes "upstream" files in "parent folder" after filtering/sorting 

source(paste0(pre_dir, "/create_covid_cohorts.R"))

# finds covariates for covid severity (pre-existing conditions) for each of the cohorts (PP+, PP-, NP+)
# written to cohort folders in g_output/covariates/...
# since the cohort folders have much smaller data, probably don't need to loop?
if(DAP=="Bordeaux"){source(paste0(pre_dir,"/covariates_detect_large.R" ))}else{
source(paste0(pre_dir,"/covariates_detect.R" ))}

# covariate timing combining to pregnancy/covid

# checks ATC dates against covid_date (first during pregnancy) and +30 days and -30 days windows
source(paste0(pre_dir, "/cov_window_exposure_function.R"))

source(paste0(pre_dir, "/trimester_drug_exposure.R"))

source(paste0(pre_dir, "/create_table_1abc.R"))






