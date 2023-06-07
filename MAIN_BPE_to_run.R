#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
# 31/1/2023

#############################
#BASIC SETUP
#working directory (default file location)
#parameters SEARCH FOR "USER INPUT" to find all actions you must take 
#create folders, and shortcuts, to access and store data
#############################

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
  path_CDM<-paste0(projectFolder,"/CDMInstances/")
  invisible(if(dir.exists(path_CDM)==F)
  {dir.create(path_CDM)})}

#USER INPUT
#if set_my_CDM<-TRUE enter the location of of your CDM 
my_path_CDM<-"text string with location of your CDM"

if(set_my_CDM==T){path_CDM<-paste0(my_path_CDM)
invisible(if(dir.exists(path_CDM)==F)
{dir.create(path_CDM)})}


source("99_path.R")
source(paste0(pre_dir, "/packages.R"))

#USER INPUT
preg_path<-preselect_folder

# USER INPUT the exact name of YOUR pregnancy algorithm output, including file extension 
preg_data<-"imputed_pregnancy.csv"

#USER INPUT
# CHOOSE one of the following (csv OR rds) by commenting out (#) the format you are not using, and un-commenting the one you are using 
# if you have a different format, please change it to .csv OR .rds

preg_format<-"csv"

# preg_format<-".RData"

#######################
#STUDY PARAMETERS######
#######################

start_study_date<-as.Date(as.character("20180101"), format = "%Y%m%d")

end_study_date<-as.Date(as.character("20211231"), format = "%Y%m%d")

pan_start_date<-as.Date(as.character("20200301"), format = "%Y%m%d")

start_covariate_window<-as.Date(as.character("20190101"), format = "%Y%m%d")



CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name


##############
#load functions
##############

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))
source(paste0(pre_dir,"/CreateSpells_function.R"))
source(paste0(pre_dir, "/CreateConceptDatasets.R"))
source(paste0(pre_dir, "/cov_trimester_function.R"))
source(paste0(pre_dir, "/cov_window_exposure_function.R"))
source(paste0(pre_dir, "/function_standard_difference.R"))

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
source(paste0(pre_dir, "/ATC_2_detect_loop.R"))

# for supplement section of report
# finds all drug utilization of ATC level 3
source(paste0(pre_dir, "/ATC_3_detect_loop.R"))

# finds all drug utilization of ATC level 4
source(paste0(pre_dir, "/ATC_4_detect_loop.R"))

# finds all drug utilization of ATC level 5
source(paste0(pre_dir, "/ATC_5_detect_loop.R"))


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

source(paste0(pre_dir, "/covid_detect_function_BPE.R"))

source(paste0(pre_dir, "/DAP_covid_detection.R"))

source(paste0(pre_dir,"/severity_detect_BPE.R"))

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

source(paste0(pre_dir, "/trimester_drug_exposure_BPE.R"))


############################
# lower ATC for supplement
# drug exposures by timing relative to COVID and pregancy
# checks ATC dates against covid_date (first during pregnancy) and +30 days and -30 days windows
#####################


#source(paste0(pre_dir, "/trimester_drug_exposure_ATC_supplement.R"))

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

###################
#timing of each covariate date against each covid/pregnancy date
###################

source(paste0(pre_dir, "/covid_covariates_timing.R"))

source(paste0(pre_dir, "/maternal_covariates_timing.R"))

source(paste0(pre_dir, "/maternal_outcomes_timing.R"))

source(paste0(pre_dir, "/neonatal_outcomes_timing.R"))

source(paste0(pre_dir, "/historical_maternal_timing.R"))

source(paste0(pre_dir, "/trimester_drug_exposure_PREG_matches.R"))

source(paste0(pre_dir, "/trimester_drug_exposure_NP_matches.R"))

###############
#TABLES
###############

source(paste0(pre_dir, "/create_table0_cohort_description.R"))


###############
#TABLE 1abc
##############

source(paste0(pre_dir, "/create_table_1abc.R"))


source(paste0(pre_dir, "/create_supplement_atc_tables.R"))


################
#TABLE 2abc
#baseline characteristics of covid+ pregnancy cohort
# tabulated and stored in g_output/final/
###############
source(paste0(pre_dir, "/create_table_2abc.R"))

source(paste0(pre_dir, "/create_table_2def.R"))

source(paste0(pre_dir, "/create_table_2abc_comorbidities.R"))

source(paste0(pre_dir, "/create_table_2abc_maternal.R"))


################
#TABLE 3
################

source(paste0(pre_dir, "/create_table_3abc.R"))

source(paste0(pre_dir, "/create_table_3def.R"))

source(paste0(pre_dir, "/create_table_3abc_comorbidities.R"))

source(paste0(pre_dir, "/create_table_3abc_maternal.R"))


##############
#TABLE 4
##############

source(paste0(pre_dir, "/create_table_4.R"))

##############
#TABLE 5
##############

# you may see this message in RED- it's OK- it just means that this outcome has no observations
# Error in prop.test(trim1[[1]], trim1[[2]]) : 
#   elements of 'n' must be positive
# In addition: Warning messages:
#   1: In prop.test(trim1[[1]], trim1[[2]]) :
#   Chi-squared approximation may be incorrect
# 2: In prop.test(trim1[[1]], trim1[[2]]) :
#   Chi-squared approximation may be incorrect

source(paste0(pre_dir, "/create_table_5_maternal_data.R"))

# when there are 0 observations- prop.test prints a warning- it's OK

source(paste0(pre_dir, "/create_table_5_mat_death.R"))

source(paste0(pre_dir, "/create_table_5_cesarean.R"))

source(paste0(pre_dir, "/create_table_5_gest_diab.R"))

source(paste0(pre_dir, "/create_table_5_preeclamp.R"))

source(paste0(pre_dir, "/create_table_5_preterm.R"))

source(paste0(pre_dir,"/create_table_5_SA_SB_coxph.R" ))

source(paste0(pre_dir,"/create_table_5_SA_SB_coxph_UNADJUSTED.R" ))

if(DAP=="SWANSEA"){
  source(paste0(pre_dir, "/create_table_5_topfa.R"))
}


# clear environment between maternal and neonatal to prevent incorrect object assignment

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

# CHECK/ FILL IN YOUR PARAMETERS 

source("params.R")

source("99_path.R")

source(paste0(pre_dir, "/neonate_mom_covariate_link.R"))

source(paste0(pre_dir, "/create_table_5_neonate_data.R"))

source(paste0(pre_dir, "/create_table_5_neonatal.R"))

#############
#TABLE 6
#############

source(paste0(pre_dir, "/create_table_6_maternal_data.R"))

# when there are 0 observations- prop.test prints a warning about Chi squared test- it's OK

source(paste0(pre_dir, "/create_table_6_gest_diab.R"))

source(paste0(pre_dir, "/create_table_6_preeclamp.R"))

source(paste0(pre_dir, "/create_table_6_preterm.R"))

# USER INPUT needed due to clear environment between maternal and neonatal to prevent incorrect object assignment

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

# CHECK/ FILL IN YOUR PARAMETERS 

############################
#PARAMETERS#################
#USER INPUT!################
############################

#USER INPUT
#if your CDM is stored in a different folder than the projectFolder set to TRUE
set_my_CDM<-FALSE

if(set_my_CDM==F){
  path_CDM<-paste0(projectFolder,"/CDMInstances/")
  invisible(if(dir.exists(path_CDM)==F)
  {dir.create(path_CDM)})}

#if set_my_CDM<-TRUE enter the location of of your CDM 
my_path_CDM<-"text string with location of your CDM"

if(set_my_CDM==T){path_CDM<-paste0(my_path_CDM)
invisible(if(dir.exists(path_CDM)==F)
{dir.create(path_CDM)})}


source("99_path.R")
source(paste0(pre_dir, "/packages.R"))

#USER INPUT where is your pregnancy algorithm output stored?
preg_path<-preselect_folder

# USER INPUT the exact name of YOUR pregnancy algorithm output, including file extension 
preg_data<-"preg_trim.csv"

#USER INPUT
# CHOOSE one of the following (csv OR rds) by commenting out (#) the format you are not using, and un-commenting the one you are using 
# if you have a different format, please change it to .csv OR .rds

preg_format<-"csv"

# preg_format<-".RData"

source("params.R")

source("99_path.R")


source(paste0(pre_dir, "/create_table_6_neonate_data.R"))

source(paste0(pre_dir, "/create_table_6_LBW.R"))

source(paste0(pre_dir, "/create_table_6_MAJORCA.R"))


#META ANANLYSIS RAW COUNTS TABLES

source(paste0(pre_dir,"/meta_table_1a.R"))


source(paste0(pre_dir,"/meta_table_2def.R"))

source(paste0(pre_dir,"/meta_table_3def.R"))











