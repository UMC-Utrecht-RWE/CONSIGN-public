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
  # PREECLAMP
  
  my_rows<-which(Reduce(`|`, lapply("P_PREECLAMP_AESI", startsWith, x = as.character(all_codes$full_name))))
  
  PREECLAMP_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(PREECLAMP_codes, startsWith, x = as.character(EVENTS$event_code))))
  PREECLAMP_EV_ID<-(EVENTS$person_id[my_rows])
  PREECLAMP_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  # UiO uses so_source_column= "eklampsia" AND so_source_value= "1"
  # OR
  # so_source_column= "preekl" AND so_source_value= "1" or "2" or "3"
  # OR
  # so_source_column= "preekltidl" AND so_source_value= "1"
  # OR
  # so_source_column= "HELLP" AND so_source_value= "1"
  # Date is 'so_date'
  
  if(DAP=="UOSL"){
    my_rows1<-which(SURV_OB$so_source_column=="eklampsia"&SURV_OB$so_source_value==1)
    my_rows2<-which(SURV_OB$so_source_column=="preekl"&SURV_OB$so_source_value%in%c(1,2,3))
    my_rows3<-which(SURV_OB$so_source_column=="preekltidl"&SURV_OB$so_source_value==1)
    my_rows4<-which(SURV_OB$so_source_column=="HELLP"&SURV_OB$so_source_value==1)
    
    my_rows<-unique(c(my_rows1, my_rows2, my_rows3, my_rows4))
    PREECLAMP_SO_ID<-(SURV_OB$person_id[my_rows])
    PREECLAMP_SO_Date<- (SURV_OB$so_date[my_rows])
  }else{PREECLAMP_SO_ID<-NA
  PREECLAMP_SO_Date<-NA}
  
  
  PREECLAMP_id<-c(PREECLAMP_EV_ID, PREECLAMP_SO_ID, PREECLAMP_PROC_ID)
  PREECLAMP_date<-c(PREECLAMP_EV_Date, PREECLAMP_SO_Date, PREECLAMP_PROC_Date)
  PREECLAMP_cov<-as.data.frame(cbind(PREECLAMP_id, PREECLAMP_date))
  
  fwrite(PREECLAMP_cov, paste0(output_folder,"PREECLAMP.csv"))}

