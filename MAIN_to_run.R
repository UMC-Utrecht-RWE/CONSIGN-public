
#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
# 31/1/2023

#############################
#BASIC SETUP
#working directory (default file location)
#parameters
#create folders, and shortcuts, to access and store data
#############################

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

source("params.R")

source("99_path.R")

source(paste0(pre_dir, "/packages.R"))

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

##############
#load functions
##############
source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))
source(paste0(pre_dir,"/CreateSpells_function.R"))
source(paste0(pre_dir, "/CreateConceptDatasets.R"))
source(paste0(pre_dir, "/covid_detect_function.R"))
source(paste0(pre_dir, "/cov_trimester_function.R"))
source(paste0(pre_dir, "/cov_window_exposure_function.R"))

##############################
#pregnancy-independent section
# create spells
# extract ATC data from MEDICINES
##############################


# STUDY POPULATION  RESULTS STORED IN CDM_preselect/
#creates new dataset of only women (sex_at_instance_creation==F, 2008<DOB>1954)
# reduce data size for next steps
source(paste0(pre_dir, "/preselect.R"))

# RESULTS STORED IN CDM_preselect/ as ALL_OBS_SPELLS.csv
# merges observation time with gap <=7 days, selects most recent observation period

source(paste0(pre_dir,"/CreateSpells.R"))

# Removes records from MEDICINES, MEDICAL_OBSERVATIONS, EVENTS and SURVEY_OBSERVATIONS before of start study period (1-1-2018)
source(paste0(pre_dir,"/preselect_study_period.R"))

# DRUG UTILIZATION:  RESULTS STORED IN G_INTERMEDIATE
#aarhus does not do this step
#Bordeaux uses different data structure

# finds all drug utilization of ATC level 2 
source(paste0(pre_dir, "/ATC_2_detect.R"))

# for supplement section of report
# finds all drug utilization of ATC level 3
source(paste0(pre_dir, "/ATC_3_detect.R"))

# finds all drug utilization of ATC level 4
source(paste0(pre_dir, "/ATC_4_detect.R"))

# finds all drug utilization of ATC level 5
source(paste0(pre_dir, "/ATC_5_detect.R"))


##########################
#PREGNANCY STEPS
#calculate start and end dates of trimesters
#apply exclusion criteria 
#group pregnancies into pandemic and historical cohorts
#record keeping of population creation (flowchart.csv -->g_intermediate)
##########################

source(paste0(pre_dir, "/trimester_create.R"))

source(paste0(pre_dir, "/pregnancy_filter.R"))

###################
#COVID steps
# find diagnoses
# group COVID dates into episodes
#assign a severity to each episode
###################

source(paste0(pre_dir, "/DAP_covid_detection.R"))

source(paste0(pre_dir,"/severity_detect.R"))

#######################
#PREGNANCY*COVID
# per person: check each covid date against each pregnancy
# if covid_date is during pregnancy, cov_trim shows which trimester the first covid_date was in
#sorts cohorts according to pregnancy and covid status
#######################
source(paste0(pre_dir, "/trimester_covid.R"))

source(paste0(pre_dir, "/create_covid_cohorts.R"))

######################
# drug exposures by timing relative to COVID and pregancy
# checks ATC dates against covid_date (first during pregnancy) and +30 days and -30 days windows
#####################

source(paste0(pre_dir, "/trimester_drug_exposure.R"))

###############
#TABLE 1abc
##############

source(paste0(pre_dir, "/create_table_1abc.R"))

############################
# lower ATC for supplement
# drug exposures by timing relative to COVID and pregancy
# checks ATC dates against covid_date (first during pregnancy) and +30 days and -30 days windows
#####################


source(paste0(pre_dir, "/trimester_drug_exposure_ATC_supplement.R"))

source(paste0(pre_dir, "/create_supplement_atc_tables.R"))

################
#TABLE 2abc
#baseline characteristics of covid+ pregnancy cohort
# tabulated and stored in g_output/final/
###############
 source(paste0(pre_dir, "/create_table_2abc.R"))


#############
#MATCHING
#COV+ PREGNANT matched 1:3 to COV+ non pregnant and pregnant COV-
# matched cohorts in projectFolder/matched/
#############

source(paste0(pre_dir,"/match_pregnant_cohorts.R"))

# matches covid positive non-pregnant women to cases (cov+preg) by age group and date of covid infection (using first date of covid infection in controls)

source(paste0(pre_dir,"/match_covid_cohorts.R"))

# script to copy over case_dates to their controls

source(paste0(pre_dir,"/matching_dates_transfer.R"))

# make CDM subset for matched cohort to be used for covariates

source(paste0(pre_dir,"/create_match_CDM.R"))

###################
#pregnancy-child linkage
#identifies which child (children) result from the cohort pregnancy
###################

source(paste0(pre_dir,"/mother_child_linkage.R" ))


