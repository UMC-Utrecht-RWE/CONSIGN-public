#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#17/10/2022

# due to wide variety of DAP specific criteria, maternal covariates will be coded separately per DAP


preg_cohort_folders<-list(hist_preg_folder,preg_match_folder, cases_match_folder, cov_match_folder)
output_folders<-list(output_mat_cov_hist, output_mat_cov_pan_neg, output_mat_cov_pan_pos, output_mat_cov_covid_pos_match)

my_preg_data<-c("my_PREG.csv", "preg_trim.csv", "preg_trim.csv", "preg_trim.csv")

all_codes<-IMPORT_PATTERN(pat="codelist_CONSIGN", dir=projectFolder)

# only events within 1 year before covid+ pregnancy start date
# filter source data events everything before Jan 1 2019 (too old to be within covid preg window)

for(i in 1:length(preg_cohort_folders)){
  
  cohort_folder<-unlist(preg_cohort_folders[i])
  output_folder<-unlist(output_folders[i])
  preg_data<-my_preg_data[i]
  df_preg<-fread(paste0(cohort_folder,preg_data))
  
  EVENTS<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir=cohort_folder)
  MED_OB<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
  SURV_OB<-IMPORT_PATTERN(pat="SURVEY_SLIM", dir=cohort_folder)
  MED<-IMPORT_PATTERN(pat="MEDICINES_SLIM", dir=cohort_folder)
  PROC<-IMPORT_PATTERN(pat="PROCEDURE", dir=cohort_folder)
  PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=cohort_folder)

  
  MED$drug_date<-MED$date_dispensing
  MED$drug_date<-as.numeric(as.Date(MED$drug_date, format="%Y%m%d"))
  #################################################################################

  
  #################################################################################
  # CAESARIAN
  
  # IACS ALSO USES PROCEDURES origin_of_procedure = "CMBD" AND procedure_code in ICD10CM
  
  
  my_event_name<-"TP_CESAREA_COV"
  
  my_file_name<-"CESAREA"
  
  CESAREA_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  CreateConceptDatasets(codesheet = CESAREA_codelist, fil=EVENTS, path = maternal_covariates_events)
  
  CESAREA_EV<-readRDS(paste0(maternal_covariates_events,my_file_name, ".rds"))
  CESAREA_EV_ID<-(CESAREA_EV$person_id)
  CESAREA_EV_Date<- (CESAREA_EV$start_date_record)
  
  my_rows<-which(PROC$origin_of_procedure=="CMBD"&PROC$procedure_code%in%CESAREA_codelist)
  CESAREA_PROC_ID<-PROC$person_id[my_rows]
  CESAREA_PROC_Date<-PROC$procedure_date[my_rows]
  
  CESAREA_ID<-c(CESAREA_EV_ID, CESAREA_PROC_ID)
  CESAREA_Date<-c(CESAREA_EV_Date, CESAREA_PROC_Date)
  
  CESAREA_cov<-as.data.frame(cbind(CESAREA_ID, CESAREA_Date))
  colnames(CESAREA_cov)<-c("id", "date")
  fwrite(CESAREA_cov, paste0(output_folder,"CESAREA.csv"))

 }

