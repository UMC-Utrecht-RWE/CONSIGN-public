#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#10/10/2022

# CONSIGN
# this script will identify the target codes for each maternal outcome (and risk factor)
  #an adverse pregnancy event is an outcome if it occurs during/after the case pregnancy 
  # and a risk factor if it is within 2 years of start_date_pregnancy of case pregnancy
# pregnancy outcomes described in Teams file CONSIGN_Variables.xlsx using version 10/10/22
# there are DAP specific data 

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))

DAP<-CDM_source$data_access_provider_name

DAP_names<-c("ARS", "FISABIO","SWANSEA","IACS","ARHUS", "BPE","KAROLINSKA", "UOSL", "TEST")

source(paste0(pre_dir, "/lookback_function.R"))
'%exclude%' <- function(x,y)!('%in%'(x,y))

preg_cohort_folders<-list(hist_preg_folder,cov_neg_pan_preg_folder, cov_pos_pan_preg_folder)
output_folders<-list(output_mat_cov_hist, output_mat_cov_pan_neg, output_mat_cov_pan_pos)

all_codes<-fread(paste0(projectFolder,"/ALL_full_codelist.csv"))

# only events within 1 year before covid+ pregnancy start date
# filter source data events everything before Jan 1 2019 (too old to be within covid preg window)

for(i in 1:length(preg_cohort_folders)){
  
cohort_folder<-unlist(preg_cohort_folders[i])
output_folder<-unlist(output_folders[i])

EVENTS<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir=cohort_folder)
MED_OB<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
SURV_OB<-IMPORT_PATTERN(pat="SURVEY_SLIM", dir=cohort_folder)
MED<-IMPORT_PATTERN(pat="MEDICINES_SLIM", dir=cohort_folder)
PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=cohort_folder)
my_PREG<- IMPORT_PATTERN(pat="preg", dir=cohort_folder)

df <- select(MED, date_dispensing, date_prescription)
drug_date<-df %>% transmute(Label = coalesce(date_dispensing, date_prescription))
drug_date<-unlist(drug_date)
drug_date<-as.Date(drug_date, format="%Y%m%d")

MED$drug_date<-as.numeric(drug_date)


#################################################################
#MATERNAL DEATH

# all_codes[all_codes$event_definition=="Maternal  death"]

# same for all DAPs : EVENTS codes tagged as P_MATERNALDEATH_AESI in VAC4EU all_codes

maternal_death_names<-"P_MATERNALDEATH_AESI"
my_rows<-which(Reduce(`|`, lapply(maternal_death_names, startsWith, x = as.character(all_codes$full_name))))

maternal_death_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(maternal_death_codes, startsWith, x = as.character(EVENTS$event_code))))
maternal_death_EV_ID<-(EVENTS$person_id[my_rows])
maternal_death_EV_Date<- (EVENTS$start_date_record[my_rows])

# for ARS (during pregnancy or within 1 year of delivery) 
# and Arthuis (during pregnancy or within 42 days of delivery)
# if PERSONS death_date is during/after pregnancy--> maternal death



if(DAP== "ARS"){
  dead_PERSONS<-PERSONS[is.na(PERSONS$year_of_death)==F,]
  dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]<-paste0(0,(dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]))
  dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]<-paste0(0,(dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]))
  dead_PERSONS$death_date<-paste0(dead_PERSONS$year_of_death, dead_PERSONS$month_of_death,dead_PERSONS$day_of_death)
  dead_PERSONS$death_date<-as.numeric(as.Date(dead_PERSONS$death_date, format="%Y%m%d"))
  
  dead_mother<-my_PREG[my_PREG$person_id%in%dead_PERSONS$person_id]
  
  maternal_death<-dead_PERSONS[between(dead_PERSONS$death_date, dead_mother$pregnancy_start_date, (dead_mother$pregnancy_end_date)+365),]
  maternal_death_pers_Date<-maternal_death$death_date
  maternal_death_pers_ID<-maternal_death$person_id
  
}

if(DAP== "ARHUS"){
  dead_PERSONS<-PERSONS[is.na(PERSONS$year_of_death)==F,]
  dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]<-paste0(0,(dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]))
  dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]<-paste0(0,(dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]))
  dead_PERSONS$death_date<-paste0(dead_PERSONS$year_of_death, dead_PERSONS$month_of_death,dead_PERSONS$day_of_death)
  dead_PERSONS$death_date<-as.numeric(as.Date(dead_PERSONS$death_date, format="%Y%m%d"))
  
  dead_mother<-my_PREG[my_PREG$person_id%in%dead_PERSONS$person_id]
  
  maternal_death<-dead_PERSONS[between(dead_PERSONS$death_date, dead_mother$pregnancy_start_date, (dead_mother$pregnancy_end_date)+42),]
  maternal_death_pers_Date<-maternal_death$death_date
  maternal_death_pers_ID<-maternal_death$person_id
  
}

no_person_death_DAP<-DAP_names[DAP_names%exclude%c("ARS", "ARHUS")]

if(DAP%in%no_person_death_DAP){maternal_death_pers_Date<-NA
maternal_death_pers_ID<-NA}

# for SWANSEA and BPE also query SURVEY table

if(DAP=="SWANSEA"){
maternal_death_so<-"adde_deaths"}
if(DAP=="BPE"){
  maternal_death_so<-"DECES"}

if(DAP%in% c("SWANSEA","BPE")){
my_rows<-which(Reduce(`|`, lapply(maternal_death_so, startsWith, x = as.character(SURV_OB$so_meaning))))
maternal_death_SO_ID<-(SURV_OB$person_id[my_rows])
maternal_death_SO_Date<- (SURV_OB$so_date[my_rows])}

no_surv_death_DAP<-DAP_names[DAP_names%exclude%c("SWANSEA", "BPE")]

if(DAP%in%no_surv_death_DAP){maternal_death_SO_Date<-NA
maternal_death_SO_ID<-NA}

maternal_death_id<-c(maternal_death_EV_ID, maternal_death_pers_ID,maternal_death_SO_ID)
maternal_death_date<-c(maternal_death_EV_Date, maternal_death_pers_Date, maternal_death_SO_Date)
maternal_death_outcome<-as.data.frame(cbind(maternal_death_id, maternal_death_date))

fwrite(maternal_death_outcome, paste0(output_folder,"maternal_death.csv"))}


#################################################################################
# GEST_DIAB

GEST_DIAB_names<-c("P_GESTDIAB_AESI")
my_rows<-which(Reduce(`|`, lapply(GEST_DIAB_names, startsWith, x = as.character(all_codes$full_name))))

GEST_DIAB_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(GEST_DIAB_codes, startsWith, x = as.character(EVENTS$event_code))))
GEST_DIAB_EV_ID<-(EVENTS$person_id[my_rows])
GEST_DIAB_EV_Date<- (EVENTS$start_date_record[my_rows])

# UiO uses SURVEY so_source_column= "diabetes_mellitus"  AND so_source_value= "4"

# BPE uses PROCEDURE

GEST_DIAB_id<-c(GEST_DIAB_EV_ID, GEST_DIAB_MO_ID, GEST_DIAB_SO_ID, GEST_DIAB_MED_ID)
GEST_DIAB_date<-c(GEST_DIAB_EV_Date, GEST_DIAB_MO_Date, GEST_DIAB_SO_Date, GEST_DIAB_MED_Date)
GEST_DIAB_cov<-as.data.frame(cbind(GEST_DIAB_id, GEST_DIAB_date))

fwrite(GEST_DIAB_cov, paste0(output_folder,"gest_diab.csv"))

