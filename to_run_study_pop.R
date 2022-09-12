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

source(paste0(pre_dir,"/CreateSpells_function.R"))


#creates new dataset of only women (sex_at_instance_creation==F, DOB>1954)
source(paste0(pre_dir, "/preselect.R"))

source(paste0(pre_dir,"/CreateSpells.R"))

# calculates trimester dates and adds these variables to original pregnancy data
source(paste0(pre_dir, "/trimester_create.R"))

#filters out red quality pregnancies
# creates subsets of pregnancy-having (historical and pandemic) and non-pregnancy-having women
source(paste0(pre_dir, "/pregnancy_filter.R"))



# add follow up after pregnancy criteria
# DAP meeting: pregnancies ending by Sept 30th 2021 (before age of omicron) AND need to determine observation time for post-natal outcome (3 months)
# end of data october +3 months (december) for drug utilization
# -2/9 CONFIRMED
# -exlcude RED (spontaneous abortion) ARS exception


# create spells (check when/if this run)



