
#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#20/9/2022

#pregnancy-independent scripts (waiting for pregnancy algorithm update)

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

source("params.R")

source("99_path.R")

source(paste0(pre_dir, "/packages.R"))


source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

# STUDY POPULATION  RESULTS STORED IN CDM_preselect/
#creates new dataset of only women (sex_at_instance_creation==F, 2008<DOB>1954)
# reduce data size for next steps
source(paste0(pre_dir, "/preselect.R"))

# RESULTS STORED IN CDM_preselect/ as ALL_OBS_SPELLS.csv
# merges observation time with gap <=7 days, selects most recent observation period
source(paste0(pre_dir,"/CreateSpells_function.R"))
source(paste0(pre_dir,"/CreateSpells.R"))


# DRUG UTILIZATION:  RESULTS STORED IN G_INTERMEDIATE

# finds all drug utilization of ATC level 2 
source(paste0(pre_dir, "/ATC_2_detect_loop.R"))

# for supplement section of report
# finds all drug utilization of ATC level 3
source(paste0(pre_dir, "/ATC_3_detect_loop.R"))

# finds all drug utilization of ATC level 4
source(paste0(pre_dir, "/ATC_4_detect_loop.R"))

# finds all drug utilization of ATC level 5
source(paste0(pre_dir, "/ATC_5_detect_loop.R"))


