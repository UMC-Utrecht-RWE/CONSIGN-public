# get procedures for BPE and IACS

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



procedures<-IMPORT_PATTERN(pat = "PROCEDURE", dir=path_CDM)

case_id<-IMPORT_PATTERN(pat="matches_cases.csv", dir=matched_folder, colls = "person_id")

cov_pos_match_id<- IMPORT_PATTERN(pat="matches_cov_pos_non_preg", dir=matched_folder, colls="person_id")

pregnant_cov_neg_match_id<-IMPORT_PATTERN(pat="matches_pregnant_cov_neg", dir=matched_folder, colls="person_id")

historical_id<-IMPORT_PATTERN(pat="my_PREG", dir=hist_preg_folder, colls="person_id")

fwrite(procedures[procedures$person_id%in%case_id,],paste0(cases_match_folder, "PROCEDURES.csv"))
fwrite(procedures[procedures$person_id%in%cov_pos_match_id,],paste0(cov_match_folder, "PROCEDURES.csv"))
fwrite(procedures[procedures$person_id%in%pregnant_cov_neg_match_id,],paste0(preg_match_folder, "PROCEDURES.csv"))
fwrite(procedures[procedures$person_id%in%historical_id,],paste0(hist_preg_folder, "PROCEDURES.csv"))

if(DAP=="IACS"){
  source(paste0(pre_dir,"/IACS_mat_cov_PROC.R" ))}

if(DAP=="Bordeaux"){
  source(paste0(pre_dir,"/Bordeaux_mat_cov_PROC.R" ))}

