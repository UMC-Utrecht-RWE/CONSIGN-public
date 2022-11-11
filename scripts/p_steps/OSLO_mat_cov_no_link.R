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
  # GEST_DIAB
  
  my_rows<-which(SURV_OB$so_source_column=="diabetes_mellitus"&SURV_OB$so_source_value==4)
  GEST_DIAB_SO_ID<-(SURV_OB$person_id[my_rows])
  GEST_DIAB_SO_Date<- (SURV_OB$so_date[my_rows])
 
  GEST_DIAB_cov<-as.data.frame(cbind(GEST_DIAB_SO_ID, GEST_DIAB_SO_Date))
  colnames(GEST_DIAB_cov)<-c("id", "date")
  fwrite(GEST_DIAB_cov, paste0(output_folder,"gest_diab.csv"))
  
  #################################################################################
  # CAESARIAN
  
  
  
  my_rows<-which(SURV_OB$so_source_column=="ksnitt"&SURV_OB$so_source_value%in%c(1,2,9))
  
  CESAREA_SO_ID<-(SURV_OB$person_id[my_rows])
  CESAREA_SO_Date<- (SURV_OB$so_date[my_rows])
 
  
  CESAREA_cov<-as.data.frame(cbind(CESAREA_SO_ID, CESAREA_SO_Date))
  colnames(CESAREA_cov)<-c("id", "date")
  fwrite(CESAREA_cov, paste0(output_folder,"CESAREA.csv"))

 #################################################################################
  # SPONTANEOUS ABORTION 
  
  # oslo so_source_column= "dodkat"  AND so_source_value= "7"or "8" or "9" so_source_column= "svlen_dg" AND so_source_value < =  "154"
  
  # Use the SURVEY_OBSERVATION table to search for this outcome using the person_id of the child or the survey_id of the pregnancy 
  # which is from the pregnancy algorithm.. The so_date for all variables in the SO table is the end date of pregnancy.
  
  # my_rows1<-which(SURV_OB$so_source_column=="dodkat"&SURV_OB$so_source_value%in%c(7,8,9))
  # 
  # my_rows2<-which(SURV_OB$so_source_column=="svlen_dg"&SURV_OB$so_source_value<=154)
  # 
  # my_rows<-unique(c(my_rows1, my_rows2))
  # 
  # SA_SO_ID<-(SURV_OB$person_id[my_rows])
  # SA_SO_Date<- (SURV_OB$so_date[my_rows])
  # 
  # 
  
  
  SA_alg_ID<-df_preg$person_id[df_preg$type_of_pregnancy_end=="SA"]
  SA_alg_Date<-df_preg$pregnancy_end_date[df_preg$type_of_pregnancy_end=="SA"]
  
  SA_ID<-c(SA_alg_ID)
  SA_Date<-c(SA_alg_Date)
  
  SA_cov<-as.data.frame(cbind(SA_ID, SA_Date))
  
  colnames(SA_cov)<-c("id", "date")
  fwrite(SA_cov, paste0(output_folder,"Spont_Abort.csv"))
  
    
#################################################################################
  # STILL BIRTH
  
  # OSLO same specs for SB as SA
  # Using SURVEY_OBSERVATIONS
  #TODO : missing specification

  
  SB_alg_ID<-df_preg$person_id[df_preg$type_of_pregnancy_end=="SB"]
  SB_alg_Date<-df_preg$pregnancy_end_date[df_preg$type_of_pregnancy_end=="SB"]
  
  SB_ID<-c(SB_alg_ID)
  SB_Date<-c(SB_alg_Date)
  
  SB_cov<-as.data.frame(cbind(SB_ID,SB_Date))
  
  fwrite(SB_cov, paste0(output_folder,"Still_Birth.csv"))
  #################################################################################
  # PREECLAMPSIA
  
  # oslo uses survey
  
  my_rows1<-which(SURV_OB$so_source_column=="eklampsia"&SURV_OB$so_source_value==1)
  my_rows2<-which(SURV_OB$so_source_column=="preekl"&SURV_OB$so_source_value%in%c(1,2,3))
  my_rows3<-which(SURV_OB$so_source_column=="preekltidl"&SURV_OB$so_source_value==1)
  my_rows4<-which(SURV_OB$so_source_column=="HELLP"&SURV_OB$so_source_value==1)
  
  my_rows<-unique(c(my_rows1, my_rows2, my_rows3, my_rows4))
  PREECLAMP_SO_ID<-(SURV_OB$person_id[my_rows])
  PREECLAMP_SO_Date<- (SURV_OB$so_date[my_rows])
  
  
  PREECLAMP_cov<-as.data.frame(cbind(PREECLAMP_SO_ID,PREECLAMP_SO_Date))
  colnames(PREECLAMP_cov)<-c("id", "date")
  fwrite(PREECLAMP_cov, paste0(output_folder,"Preeclampsia.csv"))

  #################################################################################
  # TOPFA
  # HELP
  # oslo missing this column on teams
  
  # TOPFA_names<-c("P_SUSPFETANOM_AESI","P_ELECTTERM_AESI" )
  # 
  # my_rows<-which(Reduce(`|`, lapply(TOPFA_names, startsWith, x = as.character(all_codes$full_name))))
  # 
  # TOPFA_codes<- unique(all_codes$code[my_rows])
  # 
  # my_rows<-which(Reduce(`|`, lapply(TOPFA_codes, startsWith, x = as.character(EVENTS$event_code))))
  # TOPFA_ID<-(EVENTS$person_id[my_rows])
  # TOPFA_Date<- (EVENTS$start_date_record[my_rows])
  # 
  # 
  TOPFA_cov<-as.data.frame(cbind(NA,NA))
  colnames(TOPFA_cov)<-c("id", "date") 
  fwrite(TOPFA_cov, paste0(output_folder,"TOPFA.csv"))
  
  #################################################################################
  # PRE-TERM BIRTH
  
  # OSLO only uses pregnancy algorithm output
  
  
  
  df_preg$gest_weeks<-(df_preg$pregnancy_end_date-df_preg$pregnancy_start_date)/7
  
  PRETERM_alg_ID<-df_preg$person_id[(df_preg$type_of_pregnancy_end=="LB")&(df_preg$gest_weeks<37)]
  PRETERM_alg_Date<-df_preg$pregnancy_end_date[(df_preg$type_of_pregnancy_end=="LB")&(df_preg$gest_weeks<37)]
  
  PRETERM_ID<-c( PRETERM_alg_ID)
  PRETERM_Date<-c( PRETERM_alg_Date)
  
  PRETERM_cov<-as.data.frame(cbind(PRETERM_ID,PRETERM_Date))
  colnames(PRETERM_cov)<-c("id","date")
  fwrite(PRETERM_cov, paste0(output_folder,"PRETERM.csv"))
  
  
#####################################################################

#MATERNAL DEATH

# OSLO does not report maternal death

maternal_death_id<-NA
maternal_death_date<-NA
maternal_death_outcome<-as.data.frame(cbind(maternal_death_id, maternal_death_date))
colnames(maternal_death_outcome)<- c("id", "date")
fwrite(maternal_death_outcome, paste0(output_folder,"maternal_death.csv"))

}