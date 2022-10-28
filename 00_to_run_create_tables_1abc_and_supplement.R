#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
# 7/9/2022
# updated 21/10 to fix entry and exit criteria for cohorts

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

###########################################################

source(paste0(pre_dir, "/create_table0_cohort_description.R"))

###########################################################

source(paste0(pre_dir, "/create_table_1abc.R"))

##########################################################
# lower ATC for supplement

 source(paste0(pre_dir, "/create_supplement_atc_tables.R"))

############################################################







