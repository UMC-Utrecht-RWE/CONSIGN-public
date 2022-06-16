#Author:Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 01/03/2022

#This script sets and saves paths to folders needed for all subsequent scripts
# setwd('..') #in Data Characterisation
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
# 
# setwd('..') 
# dir_base<-getwd()
# set the name of the study
StudyName <- "CONSIGN"

path_dir<-paste0(projectFolder,"/scripts/")
if(dir.exists(path_dir)==F){dir.create(path_dir)}

# path_dir<-paste0(dir_base,"/CDMInstances_preselect/") # use this option if you want to use the preselection files
# path<-path_dir

path_CDM<-paste0(projectFolder,"/CDMInstances/")
invisible(if(dir.exists(path_CDM)==F)
{dir.create(path_CDM)})

pan_preg_folder<-paste0(projectFolder,"/CDMInstances_pan_pregnant/")
invisible(if(dir.exists(pan_preg_folder)==F)
{dir.create(pan_preg_folder)})

hist_preg_folder<-paste0(projectFolder,"/CDMInstances_hist_pregnant/")
invisible(if(dir.exists(hist_preg_folder)==F)
{dir.create(hist_preg_folder)})

not_preg_folder<-paste0(projectFolder,"/CDMInstances_not_pregnant/")
invisible(if(dir.exists(not_preg_folder)==F)
{dir.create(not_preg_folder)})

preselect_folder<-paste0(projectFolder,"/CDMInstances_preselect/")
invisible(if(dir.exists(preselect_folder)==F)
{dir.create(preselect_folder)})

# Checks if folders exist. If they do not, creates them 
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/")), dir.create(paste0(projectFolder, "/g_intermediate")), FALSE))
g_intermediate <- paste0(projectFolder, "/g_intermediate/")
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
output_dir     <- paste0(projectFolder, "/g_output/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/drugs/")), dir.create(paste0(projectFolder, "/g_output/drugs")), FALSE))
output_drugs    <- paste0(projectFolder, "/g_output/drugs/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/trimester/")), dir.create(paste0(projectFolder, "/g_output/trimester")), FALSE))
output_trimester    <- paste0(projectFolder, "/g_output/trimester/")

# Sets path to p_steps (to read codelists)
pre_dir <- paste0(path_dir,"p_steps")
invisible(ifelse(!dir.exists(paste0(path_dir, "p_steps")), dir.create(paste0(path_dir, "p_steps")), FALSE))
# folders + paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "populations")), dir.create(paste0(g_intermediate, "populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "tmp", sep="")), dir.create(paste0(g_intermediate, "tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")



