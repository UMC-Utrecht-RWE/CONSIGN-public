#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#30/5/2022

# to run this script you must have pregnancy algorithm output and enter the file name in params.r

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

#user input in params.R

source("99_path.R")

source(paste0(pre_dir, "/packages.R"))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

source(paste0(pre_dir,"/params.R"))

# merges observation time with gap <=7 days, selects most recent observation period
source(paste0(pre_dir,"/CreateSpells_function.R"))
source(paste0(pre_dir,"/CreateSpells.R"))

#creates new dataset of only women (sex_at_instance_creation==F, 2008<DOB>1954)
source(paste0(pre_dir, "/preselect.R"))

# calculates trimester dates and adds these variables to original pregnancy data
source(paste0(pre_dir, "/trimester_create.R"))

#filters out red quality pregnancies
# creates subsets of pregnancy-having (historical and pandemic) and non-pregnancy-having women
source(paste0(pre_dir, "/pregnancy_filter.R"))




