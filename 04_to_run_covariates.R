#Author: Ema Alsina, M.Sc.
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


# # finds covariates for covid severity (pre-existing conditions) for each of the cohorts (PP+, PP-, NP+)
# # written to cohort folders in g_output/covariates/...
# # since the cohort folders have much smaller data, probably don't need to loop?

if(DAP=="Bordeaux"){source(paste0(pre_dir,"/covariates_detect_large.R" ))}else{
  source(paste0(pre_dir,"/covariates_detect.R" ))}
# 
# covariate timing combining to pregnancy/covid


# 
# source(paste0(pre_dir,"/maternal_death_detect.R" ))
# 
# source(paste0(pre_dir,"/maternal_gestdiab_detect.R" ))
# 
# source(paste0(pre_dir,"/maternal_preeclamp_detect.R" ))
