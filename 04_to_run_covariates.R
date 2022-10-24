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


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

if(DAP=="ARS"){
  source(paste0(pre_dir,"/ARS_mat_cov_no_link.R" ))}
  
if(DAP=="Aarhus"){
  source(paste0(pre_dir,"/Aarhus_mat_cov_no_link.R" ))}

if(DAP=="Bordeaux"){
  source(paste0(pre_dir,"/Bordeaux_mat_cov_no_link.R" ))}

if(DAP=="FISABIO"){
  source(paste0(pre_dir,"/Fisabio_mat_cov_no_link.R" ))}

if(DAP=="IACS"){
  source(paste0(pre_dir,"/IACS_mat_cov_no_link.R" ))}

if(DAP=="Karolinska"){
  source(paste0(pre_dir,"/Karolinska_mat_cov_no_link.R" ))}

if(DAP=="UOSL"){
  source(paste0(pre_dir,"/Oslo_mat_cov_no_link.R" ))}

if(DAP=="USWAN"){
  source(paste0(pre_dir,"/SWANSEA_mat_cov_no_link.R" ))}
