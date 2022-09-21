#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#20/9/2022

#pregnancy-DEPENDENT scripts (waiting for pregnancy algorithm update)

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)


source("99_path.R")

source(paste0(pre_dir, "/packages.R"))

source(paste0(pre_dir,"/params.R"))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

# calculates trimester dates AND maternal age categories and adds these variables to original pregnancy data
source(paste0(pre_dir, "/trimester_create.R"))

#filters out red quality pregnancies
# creates subsets of pregnancy-having (historical and pandemic) and non-pregnancy-having women

# NEED TO ADD FILTER FOR PERSON_ID%IN% ALL_OBS (OUTPUT OF CREATE SPELLS)
# NEED TO ADD AGE FILTER FOR HISTORICAL VS. PANDEMIC GROUPS

source(paste0(pre_dir, "/pregnancy_filter.R"))

source(paste0(pre_dir, "/cov_trimester_function.R"))

source(paste0(pre_dir, "/trimester_covid.R"))


source(paste0(pre_dir, "/create_covid_cohorts.R"))

source(paste0(pre_dir, "/cov_window_exposure_function.R"))

source(paste0(pre_dir, "/trimester_drug_exposure.R"))





