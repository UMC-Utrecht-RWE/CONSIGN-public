#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#20/9/2022
#further development 5/10/22

#pregnancy-DEPENDENT scripts (waiting for pregnancy algorithm update)

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

source("params.R")

source("99_path.R")

suppressMessages(source(paste0(pre_dir, "/packages.R")))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

# adding variables to pregnancy data
# calculates trimester dates AND maternal age categories and adds these variables to original pregnancy data
source(paste0(pre_dir, "/trimester_create.R"))

#applying exclusion criteria and creating pregnancy cohorts 
    #filters out red quality pregnancies
    #checks pregnancies have 12 months follow up from preg_start_date
    # creates subsets of pregnancy-having (historical and pandemic) and non-pregnancy-having women

  #when pregnancy cohorts are created, the preselect folder is deleted

source(paste0(pre_dir, "/pregnancy_filter.R"))

# tests covid_dates against pregnancy dates
# if covid_date is during pregnancy, cov_trim shows which trimester the first covid_date was in

source(paste0(pre_dir, "/cov_trimester_function.R"))

source(paste0(pre_dir, "/trimester_covid.R"))

#sorts cohorts according to pregnancy and covid status
# deletes "upstream" files in "parent folder" after filtering/sorting 

source(paste0(pre_dir, "/create_covid_cohorts.R"))

# checks ATC dates against covid_date (first during pregnancy) and +30 days and -30 days windows
source(paste0(pre_dir, "/cov_window_exposure_function.R"))

source(paste0(pre_dir, "/trimester_drug_exposure.R"))


source(paste0(pre_dir, "/create_table_1abc.R"))






