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

# source(paste0(pre_dir, "/covid_covariates_timing.R"))

source(paste0(pre_dir, "/maternal_covariates_timing_AARHUS.R"))

# source(paste0(pre_dir, "/maternal_outcomes_timing.R"))

source(paste0(pre_dir, "/neonatal_outcomes_timing_AARHUS.R"))

source(paste0(pre_dir, "create_table_4"))


