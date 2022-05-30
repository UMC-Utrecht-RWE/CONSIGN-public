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

#creates new dataset of only women (sex_at_instance_creation==F, DOB>1954)
source(paste0(pre_dir, "/preselect.R"))
#creates subsets of pregnancy-having and non-pregnancy-having women
source(paste0(pre_dir, "/pregnancy_filter.R"))

source(paste0(pre_dir, "/ATC_detect.R"))

source(paste0(pre_dir, "/ATC_COVID_dates.R"))
