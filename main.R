#                                                                                                
#                                                                                                
#  UUUUUUUU     UUUUUUUUMMMMMMMM               MMMMMMMM        CCCCCCCCCCCCCUUUUUUUU     UUUUUUUU
#  U::::::U     U::::::UM:::::::M             M:::::::M     CCC::::::::::::CU::::::U     U::::::U
#  U::::::U     U::::::UM::::::::M           M::::::::M   CC:::::::::::::::CU::::::U     U::::::U
#  UU:::::U     U:::::UUM:::::::::M         M:::::::::M  C:::::CCCCCCCC::::CUU:::::U     U:::::UU
#   U:::::U     U:::::U M::::::::::M       M::::::::::M C:::::C       CCCCCC U:::::U     U:::::U 
#   U:::::D     D:::::U M:::::::::::M     M:::::::::::MC:::::C               U:::::D     D:::::U 
#   U:::::D     D:::::U M:::::::M::::M   M::::M:::::::MC:::::C               U:::::D     D:::::U 
#   U:::::D     D:::::U M::::::M M::::M M::::M M::::::MC:::::C               U:::::D     D:::::U 
#   U:::::D     D:::::U M::::::M  M::::M::::M  M::::::MC:::::C               U:::::D     D:::::U 
#   U:::::D     D:::::U M::::::M   M:::::::M   M::::::MC:::::C               U:::::D     D:::::U 
#   U:::::D     D:::::U M::::::M    M:::::M    M::::::MC:::::C               U:::::D     D:::::U 
#   U::::::U   U::::::U M::::::M     MMMMM     M::::::M C:::::C       CCCCCC U::::::U   U::::::U 
#   U:::::::UUU:::::::U M::::::M               M::::::M  C:::::CCCCCCCC::::C U:::::::UUU:::::::U 
#    UU:::::::::::::UU  M::::::M               M::::::M   CC:::::::::::::::C  UU:::::::::::::UU  
#      UU:::::::::UU    M::::::M               M::::::M     CCC::::::::::::C    UU:::::::::UU    
#        UUUUUUUUU      MMMMMMMM               MMMMMMMM        CCCCCCCCCCCCC      UUUUUUUUU      
#                                                                                                
#        ****************       REAL WORLD EVIDENCE PIPELINE        *******Ver. 2.1********                                                                                           
#                            University Medical Center Utrecht
# 
#                                                                                                
# Project: CONSIGN                                                                                               
# Contact: e.m.alsina-2@umcutrecht.nl , t.andresvaz@umcutrecht.nl                                                                                               
# Creation date: 22/07/2022
# See authors, license and other details at: https://github.com/UMC-Utrecht-RWE/CONSIGN
#
# GUIDELINES: Names and coding conventions.
# Files names 
# All names should be meaningful and end in .R.
# Following this standard:
# -> Files: underscore_separated, all lower case: e.g. numeric_version
# -> Functions: period.separated, all lower case: e.g. my.function
# -> Variables: lowerCamelCase: e.g. addTaskCall

# Files organisation
# -> They way files are organised has a significant impact on readability.
# -> Always consider what working directory you are in when sourcing a script.
# -> /root (to_run.R or main.R)  
# -> /source (all scripts implementing pipeline steps, one file =  one specific purpose)
# -> /function (for all files that are used by other files).
# -> /data (all CDM Instances, pre-selecions and other .csv or SQL light database files)

#Start environment and load packages
rm(list=ls())
studyName <- "CONSIGN"
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)
if(!require(data.table)){install.packages("data.table")}
library(data.table)


#Sets paths to folders to be used by all other scripts

#Pipeline source code folder
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
#TODO change to /source/
path_dir<-paste0(projectFolder,"/src/")  
if(dir.exists(path_dir)==F){dir.create(path_dir)}

#Source Data folder
pathCDM<-paste0(projectFolder,"/data/CDMInstances/")
invisible(if(dir.exists(pathCDM)==F)
{dir.create(pathCDM)})

#Project specific cohorts data folder
pan_preg_folder<-paste0(projectFolder,"/data/CDMInstances_pan_pregnant/")
invisible(if(dir.exists(pan_preg_folder)==F)
{dir.create(pan_preg_folder)})

hist_preg_folder<-paste0(projectFolder,"/data/CDMInstances_hist_pregnant/")
invisible(if(dir.exists(hist_preg_folder)==F)
{dir.create(hist_preg_folder)})

not_preg_folder<-paste0(projectFolder,"/data/CDMInstances_not_pregnant/")
invisible(if(dir.exists(not_preg_folder)==F)
{dir.create(not_preg_folder)})

preselect_folder<-paste0(projectFolder,"/data/CDMInstances_preselect/")
invisible(if(dir.exists(preselect_folder)==F)
{dir.create(preselect_folder)})

#Intermediate and output folders 
#Checks if folders exist. If they do not, creates them 
invisible(ifelse(!dir.exists(paste0(projectFolder, "/output/g_intermediate/")), dir.create(paste0(projectFolder, "/g_intermediate")), FALSE))
g_intermediate <- paste0(projectFolder, "/output/g_intermediate/")
invisible(ifelse(!dir.exists(paste0(projectFolder, "/output/g_output/")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
output_dir     <- paste0(projectFolder, "/output/g_output/")

#Project specific outputs (declare all new necessary project specific paths here)
invisible(ifelse(!dir.exists(paste0(projectFolder, "/output/g_output/atc_counts/")), dir.create(paste0(projectFolder, "/g_output/atc_counts")), FALSE))
output_drugs    <- paste0(projectFolder, "/output/g_output/atc_counts/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/output/g_output/cov_window_atc/")), dir.create(paste0(projectFolder, "/g_output/cov_window_atc")), FALSE))
output_cov_window_drugs    <- paste0(projectFolder, "/output/g_output/cov_window_atc/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/output/g_output/trimester/")), dir.create(paste0(projectFolder, "/g_output/trimester")), FALSE))
output_trimester    <- paste0(projectFolder, "/output/g_output/trimester/")

#Set additional paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "populations")), dir.create(paste0(g_intermediate, "populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "tmp", sep="")), dir.create(paste0(g_intermediate, "tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")

# Environment Loaded 

#  Now running ...
#   ______  _                _   _               
#  (_____ \(_)              | | (_)              
#   _____) )_  ____   _____ | |  _  ____   _____ 
#  |  ____/| ||  _ \ | ___ || | | ||  _ \ | ___ |
#  | |     | || |_| || ____|| | | || | | || ____|
#  |_|     |_||  __/ |_____) \_)|_||_| |_||_____)
#             |_|                                
# TO EXECUTE ALL ORDERED STEPS TO PROCESS CDM DATA 

#creates new dataset of only women (sex_at_instance_creation==F, DOB>1954)
source(paste0(path_dir, "/preselect.R"))

#calculates trimester dates and adds these variables to original pregnancy data
source(paste0(path_dir, "/trimester_create.R"))

#filters out red quality pregnancies
#creates subsets of pregnancy-having (historical and pandemic) and non-pregnancy-having women
source(paste0(path_dir, "/pregnancy_filter.R"))

#select medicine
source(paste0(path_dir, "/ATC_detect.R"))

#select covid diagnostics and dates
source(paste0(path_dir, "/simulate_cov_dates.R"))

#select pregnancy trimesters
source(paste0(path_dir, "/cov_trimester_function.R"))
source(paste0(path_dir, "/trimester_covid.R"))

#select exposure
source(paste0(path_dir, "/cov_window_exposure_function.R"))

#select drug exposure
source(paste0(path_dir, "/trimester_drug_exposure.R"))
