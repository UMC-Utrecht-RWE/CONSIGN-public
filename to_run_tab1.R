#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#30/5/2022

#CONSIGN to.run tables 1.a and 1.b

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)


source("99_path.R")

source(paste0(pre_dir, "/packages.R"))

source(paste0(pre_dir,"/params.R"))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

# find covid from EVENTS (pfijzer algorithm)
source(paste0(pre_dir, "/covid_detect_function.R"))

# find covid from survey and medical observations with DAP specific criteria
source(paste0(pre_dir, "/DAP_covid_detection.R"))

# merge events and dap-specific covid data, detect severity codes and group dates into episodes and create episode severity
source(paste0(pre_dir, "/severity_detect.R"))

# finds first covid_date to occur within pregnancy, and determines which trimester
source(paste0(pre_dir, "/cov_trimester_function.R"))

# applies function: finds first covid_date to occur within pregnancy, and determines which trimester
source(paste0(pre_dir, "/trimester_covid.R"))

# sorts into cov+preg+, cov-preg+, cov+preg- (for matching)
source(paste0(pre_dir, "/create_covid_cohorts.R"))

# finds all drug utilization of ATC level 2
source(paste0(pre_dir, "/ATC_detect.R"))

# determines if any drug in each ATC group was used within +30 or -30 day window of covid diagnosis
source(paste0(pre_dir, "/cov_window_exposure_function.R"))

# measures if drug was used during trimester of covid exposure
source(paste0(pre_dir, "/trimester_drug_exposure.R"))





