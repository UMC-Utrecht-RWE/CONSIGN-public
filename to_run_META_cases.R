rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

############################
#PARAMETERS#################
#USER INPUT!################
############################

#USER INPUT
#if your CDM is stored in a different folder than the projectFolder set to TRUE
set_my_CDM<-FALSE

if(set_my_CDM==F){
  path_CDM<-paste0(projectFolder,"/CDM/")
  invisible(if(dir.exists(path_CDM)==F)
  {dir.create(path_CDM)})}

#if set_my_CDM<-TRUE enter the location of of your CDM 
my_path_CDM<-"text string with location of your CDM"

if(set_my_CDM==T){path_CDM<-paste0(my_path_CDM)
invisible(if(dir.exists(path_CDM)==F)
{dir.create(path_CDM)})}


source("99_path.R")
source(paste0(pre_dir, "/packages.R"))

source(paste0(pre_dir,"/meta_table_MATCH_CASE_def.R"))

source(paste0(pre_dir,"/matching_report.R"))
