#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#16/10/2022

# COVARIATE

# make sure you have codelist_CONSIGN.csv in projectFolder

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

source(paste0(pre_dir, "/CreateConceptDatasets.R"))

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

source(paste0(pre_dir,"/mother_child_linkage_AARHUS.R" ))


# covariate timing combining to pregnancy/covid

# source(paste0(pre_dir,"/preterm_birth.R" ))


  source(paste0(pre_dir,"/Aarhus_mat_cov_no_link.R" ))

  source(paste0(pre_dir,"/Aarhus_neonatal_cov.R" ))

