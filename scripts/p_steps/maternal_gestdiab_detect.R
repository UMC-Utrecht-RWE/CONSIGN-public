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
PROC<-IMPORT_PATTERN(pat="PROCEDURE", dir=cohort_folder)
PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=cohort_folder)
my_PREG<- IMPORT_PATTERN(pat="preg", dir=cohort_folder)

df <- select(MED, date_dispensing, date_prescription)
drug_date<-df %>% transmute(Label = coalesce(date_dispensing, date_prescription))
drug_date<-unlist(drug_date)
drug_date<-as.Date(drug_date, format="%Y%m%d")

MED$drug_date<-as.numeric(drug_date)


#################################################################################
# GEST_DIAB

GEST_DIAB_names<-c("P_GESTDIAB_AESI")
my_rows<-which(Reduce(`|`, lapply(GEST_DIAB_names, startsWith, x = as.character(all_codes$full_name))))

GEST_DIAB_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(GEST_DIAB_codes, startsWith, x = as.character(EVENTS$event_code))))
GEST_DIAB_EV_ID<-(EVENTS$person_id[my_rows])
GEST_DIAB_EV_Date<- (EVENTS$start_date_record[my_rows])

# UiO uses SURVEY so_source_column= "diabetes_mellitus"  AND so_source_value= "4"

if(DAP=="UOSL"){
  my_rows<-which(SURV_OB$so_source_column=="diabetes_mellitus"&SURV_OB$so_source_value==4)
  GEST_DIAB_SO_ID<-(SURV_OB$person_id[my_rows])
  GEST_DIAB_SO_Date<- (SURV_OB$so_date[my_rows])
}else{GEST_DIAB_SO_ID<-NA
GEST_DIAB_SO_Date<-NA}

# BPE uses PROCEDURE origin_of_procedure = "BIOLOGIE" and procedure_code="0412"

if(DAP=="BPE"){
  my_rows<-which(PROC$origin_of_procedure == "BIOLOGIE" &PROC$procedure_code == "0412")
  GEST_DIAB_PROC_ID<-(PROC$person_id[my_rows])
  GEST_DIAB_PROC_Date<- (PROC$procedure_date[my_rows])
}else{GEST_DIAB_PROC_ID<-NA
GEST_DIAB_PROC_Date<-NA}

GEST_DIAB_id<-c(GEST_DIAB_EV_ID, GEST_DIAB_SO_ID, GEST_DIAB_PROC_ID)
GEST_DIAB_date<-c(GEST_DIAB_EV_Date, GEST_DIAB_SO_Date, GEST_DIAB_PROC_Date)
GEST_DIAB_cov<-as.data.frame(cbind(GEST_DIAB_id, GEST_DIAB_date))

fwrite(GEST_DIAB_cov, paste0(output_folder,"gest_diab.csv"))}

