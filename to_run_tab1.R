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

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

source(paste0(pre_dir, "/ATC_detect.R"))

source(paste0(pre_dir, "/covid_detect_function.R"))

source(paste0(pre_dir, "/sim_severity.R"))

source(paste0(pre_dir, "/cov_trimester_function.R"))

source(paste0(pre_dir, "/trimester_covid.R"))

source(paste0(pre_dir, "/cov_window_exposure_function.R"))

source(paste0(pre_dir, "/trimester_drug_exposure.R"))
