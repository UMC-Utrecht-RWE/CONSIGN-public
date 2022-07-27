#                                                                                       
#                                                                                       
#  RRRRRRRRRRRRRRRRR   WWWWWWWW                           WWWWWWWUEEEEEEEEEEEEEEEEEEEEE
#  R::::::::::::::::R  W::::::W                           W::::::M::::::::::::::::::::E
#  R::::::RRRRRR:::::R W::::::W                           W::::::C::::::::::::::::::::E
#  RR:::::R     R:::::UU::::::W                           W::::::UE::::::EEEEEEEEE::::E
#    R::::R     R:::::R W:::::W           WWWW            W:::::W E:::::E       EEEEEE
#    R::::R     R:::::R  W:::::W         W:::::W         W:::::W  E:::::E             
#    R::::RRRRRR:::::R    W:::::W       W:::::::W       W:::::W   E::::::EEEEEEEEEE   
#    R:::::::::::::RR      W:::::W     W:::::::::W     W:::::W    E:::::::::::::::E   
#    R::::RRRRRR:::::R      W:::::W   W:::::W:::::W   W:::::W     E:::::::::::::::E   
#    R::::R     R:::::R      W:::::W W:::::W W:::::W W:::::W      E::::::EEEEEEEEEE   
#    R::::R     R:::::R       W:::::W:::::W   W:::::W:::::W       E:::::E             
#    R::::R     R:::::R        W:::::::::W     W:::::::::W        E:::::E       EEEEEE
#  RR:::::R     R:::::R         W:::::::W       W:::::::W       EE::::::EEEEEEEE:::::E
#  R::::::R     R:::::R          W:::::W         W:::::W        E::::::::::::::::::::E
#  R::::::R     R:::::R           W:::W           W:::W         E::::::::::::::::::::E
#  RRRRRRRR     RRRRRRR            WWW             WWW          EEEEEEEEEEEEEEEEEEEEEE
#                                                                                                
#      ****************       REAL WORLD EVIDENCE PIPELINE      *******Ver. 2.1*******                                                                                           
#                                                                                                
# Project: CONSIGN 
# R code developed by University Medical Center Utrecht, Datascience and Bioestatistics, Real World Evidence Dept.
# Contacts: e.m.alsina-2@umcutrecht.nl , t.andresvaz@umcutrecht.nl                                                                                               
# Creation date: 22/07/2022
# See authors, license and other details at: https://github.com/UMC-Utrecht-RWE/CONSIGN/
#
# GUIDELINES: Names and coding conventions.
# This project is based on the ConcePTION project, for coding we use the following basic instructions.
# Core value: all file names should be meaningful. 
# Following this standard:
# -> Files: underscore_separated, all upper case: i.e. MEDICAL_OBSERVATIONS.csv
# -> Functions: period.separated, all lower case: i.e. save.files
# -> Variables: lowerCamelCase: i.e. pathToFile

# Files organisation
# -> They way files are organised has impacts in understanding of the code.
# -> Always consider one single working directory when creating a new script.
# -> /source (all scripts implementing pipeline steps, one file =  one specific purpose)
# -> /function (for all files that are used by other files).
# -> /data (all CDM Instances, pre-selecions and other .csv or SQL light database files)
#
# Comments and TODOs
# -> Review your comments and TODOs with a colleague before push
# -> Comments should explain why the code exists, not what you are trying to code.
# -> One single line of comment every one single new command is the best you can do commenting you code. 
# -> But if you have similar lines of code, organize your comments around one well determined commented block.
# -> Every TODO should be assigned to one issue in Github (before create a new issue, check for existing ones).
#
# Release notes:
#    First release including this GUIDELINES.
#    New main.R (it replaces the to_run.R file and other artifacts into one single starting point)
#    New masterfile to search for variables, including new functions. (Issue #2)
#    Implementation of study cohorts (Issue #1)
# -------------------------------------------------------------------------------------------------


# Start environment and load packages
rm(list=ls())
studyName <- "CONSIGN"
if(!require(rstudioapi)){install.packages("rstudioapi")}
suppressPackageStartupMessages(library(rstudioapi))
if(!require(data.table)){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))
if(!require(dplyr)){install.packages("dplyr")}
suppressPackageStartupMessages(library(dplyr))
if(!require(lubridate)){install.packages("lubridate")}
suppressPackageStartupMessages(library(lubridate))
if(!require(tidyverse)){install.packages("tidyverse")}
suppressPackageStartupMessages(library(tidyverse))
if(!require(tidyselect)){install.packages("tidyselect")}
suppressPackageStartupMessages(library(tidyselect))
if(!require(reshape)){install.packages("reshape")}
suppressPackageStartupMessages(library(reshape))
if(!require(rlist)){install.packages("rlist")}
suppressPackageStartupMessages(library(rlist))


#Set pipeline path to source code folder directory (to be used by all other folder definition)
projectDir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectDir)

#Set source folder for .R scripts
sourceDir<-paste0(projectDir,"/src/")  
if(dir.exists(sourceDir)==F){dir.create(sourceDir)}

#Set source Data folder
pathCDM<-paste0(projectDir,"/data/CDMInstances/")
invisible(if(dir.exists(pathCDM)==F)
{dir.create(pathCDM)})

#Project specific cohorts data folder
#1 - Pregnancy during pandemics
pan_preg_folder<-paste0(projectDir,"/data/CDMInstances_pan_pregnant/")
invisible(if(dir.exists(pan_preg_folder)==F)
{dir.create(pan_preg_folder)})
#2 - Pregnancy before pandemics
hist_preg_folder<-paste0(projectDir,"/data/CDMInstances_hist_pregnant/")
invisible(if(dir.exists(hist_preg_folder)==F)
{dir.create(hist_preg_folder)})
#3 - Not pregnant women
not_preg_folder<-paste0(projectDir,"/data/CDMInstances_not_pregnant/")
invisible(if(dir.exists(not_preg_folder)==F)
{dir.create(not_preg_folder)})
#4 - Preselect only women and age group
preselect_folder<-paste0(projectDir,"/data/CDMInstances_preselect/")
invisible(if(dir.exists(preselect_folder)==F)
{dir.create(preselect_folder)})

#Intermediate and output folders 
#Checks if folders exist. If they do not, creates them 
invisible(ifelse(!dir.exists(paste0(projectDir, "/output/g_intermediate/")), dir.create(paste0(projectDir, "/g_intermediate")), FALSE))
g_intermediate <- paste0(projectDir, "/output/g_intermediate/")
invisible(ifelse(!dir.exists(paste0(projectDir, "/output/g_output/")), dir.create(paste0(projectDir, "/g_output")), FALSE))
output_dir     <- paste0(projectDir, "/output/g_output/")

#Project specific outputs 
#-> Declare all new necessary project specific output paths here)
invisible(ifelse(!dir.exists(paste0(projectDir, "/output/g_output/atc_counts/")), dir.create(paste0(projectDir, "/g_output/atc_counts")), FALSE))
output_drugs    <- paste0(projectDir, "/output/g_output/atc_counts/")

invisible(ifelse(!dir.exists(paste0(projectDir, "/output/g_output/cov_window_atc/")), dir.create(paste0(projectDir, "/g_output/cov_window_atc")), FALSE))
output_cov_window_drugs    <- paste0(projectDir, "/output/g_output/cov_window_atc/")

invisible(ifelse(!dir.exists(paste0(projectDir, "/output/g_output/trimester/")), dir.create(paste0(projectDir, "/g_output/trimester")), FALSE))
output_trimester    <- paste0(projectDir, "/output/g_output/trimester/")

#Project specific additional paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "populations")), dir.create(paste0(g_intermediate, "populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "tmp", sep="")), dir.create(paste0(g_intermediate, "tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")

# Environment Loaded 

#  Now running ...
#   ______                   _                  
#  |_____ \                 | |                
#   _____| |_  ____   _____ | |  _  ____   _____ 
#  |  ____/| ||  _ \ | ___ || | | ||  _ \ | ___ |
#  | |     | || |_| || ____|| | | || | | || ____|
#  |_|     |_||  __/ |_____) \_)|_||_| |_||_____)
#             |_|                                
#  REAL WORLD EVIDENCE STEPS TO PROCESS CDM DATA 

#benchmarking starting time 
start_time <- Sys.time()

#creates new dataset of only women (sex_at_instance_creation==F, DOB>1954)
source(paste0(sourceDir, "/preselect.R"))

#calculates trimester dates and adds these variables to original pregnancy data
source(paste0(sourceDir, "/trimester_create.R"))

#filters out red quality pregnancies
#creates subsets of pregnancy-having (historical and pandemic) and non-pregnancy-having women
source(paste0(sourceDir, "/pregnancy_filter.R"))

#select medicine
source(paste0(sourceDir, "/ATC_detect.R"))

#select covid diagnostics and dates
source(paste0(sourceDir, "/simulate_cov_dates.R"))

#select pregnancy trimesters
source(paste0(sourceDir, "/cov_trimester_function.R"))
source(paste0(sourceDir, "/trimester_covid.R"))

#select exposure
source(paste0(sourceDir, "/cov_window_exposure_function.R"))

#select drug exposure
# commented due to error: 'list' object cannot be coerced to type 'double'
#source(paste0(sourceDir, "/trimester_drug_exposure.R"))

#compute total running time
end_time <- Sys.time()
end_time - start_time
#FINISHED WITH SUCCESS
