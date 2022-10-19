#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
# 7/9/2022

# make sure you have covid_data.csv in preselect_folder
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

# re-run covid detection scripts using updated codelist and severity indicators

source(paste0(pre_dir, "/covid_detect_function.R"))

source(paste0(pre_dir, "/DAP_covid_detection.R"))

source(paste0(pre_dir,"/severity_detect.R"))

# calculates if death occurred within 28 days of covid date using DOD generated in CreateEntryExit
source(paste0(pre_dir,"/covid_death.R"))


# tests covid_dates against pregnancy dates
# if covid_date is during pregnancy, cov_trim shows which trimester the first covid_date was in

source(paste0(pre_dir, "/cov_trimester_function.R"))

source(paste0(pre_dir, "/trimester_covid.R"))

#sorts cohorts according to pregnancy and covid status

source(paste0(pre_dir, "/create_covid_cohorts.R"))

# checks ATC dates against covid_date (first during pregnancy) and +30 days and -30 days windows
source(paste0(pre_dir, "/cov_window_exposure_function.R"))

source(paste0(pre_dir, "/trimester_drug_exposure.R"))

source(paste0(pre_dir, "/create_table_1abc.R"))

##########################################################
# lower ATC for supplement

source(paste0(pre_dir, "/trimester_drug_exposure_ATC_supplement.R"))

source(paste0(pre_dir, "/create_supplement_atc_tables.R"))







