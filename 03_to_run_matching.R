#Author: Ema Alsina, M.Sc. and Tiago PhD
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#16/10/2022

# COVARIATE

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

source(paste0(pre_dir,"/match_pregnant_cohorts.R"))

source(paste0(pre_dir,"/match_covid_cohorts.R"))

# script to copy over case_dates to their controls

source(paste0(pre_dir,"/create_matched_cohorts.R"))

