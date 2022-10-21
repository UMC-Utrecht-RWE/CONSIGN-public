#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#17/10/2022

# due to wide variety of DAP specific criteria, maternal covariates will be coded separately per DAP

preg_cohort_folders<-list(hist_preg_folder,cov_neg_pan_preg_folder, cov_pos_pan_preg_folder)
output_folders<-list(output_mat_cov_hist, output_mat_cov_pan_neg, output_mat_cov_pan_pos)

my_preg_data<-c("my_PREG.csv", "cov_neg_preg.csv", "cov_pos_preg.csv")

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
  my_PREG<- IMPORT_PATTERN(pat=my_preg_data[i], dir=cohort_folder)
  
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
 
  GEST_DIAB_cov<-as.data.frame(cbind(GEST_DIAB_EV_ID, GEST_DIAB_EV_Date))
  colnames(GEST_DIAB_cov)<-c("id", "date")
  fwrite(GEST_DIAB_cov, paste0(output_folder,"gest_diab.csv"))
  
  #################################################################################
  # CAESARIAN
  
  # Aarhus uses EVENTS and SURVEY_OB 
  # so_source_column= "KEJSERSNIT_MODERSOENSKE"  or so_source_column="Markoer_kejsersnit"  AND  so_source_value= procedure code/non NA
  
  
  my_rows<-which(Reduce(`|`, lapply("TP_CESAREA_COV", startsWith, x = as.character(all_codes$full_name))))
  
  CESAREA_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(CESAREA_codes, startsWith, x = as.character(EVENTS$event_code))))
  CESAREA_EV_ID<-(EVENTS$person_id[my_rows])
  CESAREA_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  my_rows<-which(SURV_OB$so_source_column%in%c("KEJSERSNIT_MODERSOENSKE", "Markoer_kejsersnit")& (is.na(SURV_OB$so_source_value)==F))
  CESAREA_SO_ID<-SO$person_id[my_rows]
  CESAREA_SO_Date<-SO$so_date[my_rows]
  
  CESAREA_ID<-c(CESAREA_EV_ID, CESAREA_SO_ID)
  CESAREA_Date<-c(CESAREA_EV_Date, CESAREA_SO_Date)
  
  CESAREA_cov<-as.data.frame(cbind(CESAREA_ID, CESAREA_Date))
  colnames(CESAREA_cov)<-c("id", "date")
  fwrite(CESAREA_cov, paste0(output_folder,"CESAREA.csv"))

 #################################################################################
  # SPONTANEOUS ABORTION
  
  # Aarhus USES events only
  
  my_rows<-which(Reduce(`|`, lapply("P_SPONTABO_AESI", startsWith, x = as.character(all_codes$full_name))))
  
  SA_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(SA_codes, startsWith, x = as.character(EVENTS$event_code))))
  SA_EV_ID<-(EVENTS$person_id[my_rows])
  SA_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  SA_cov<-as.data.frame(cbind(SA_EV_ID, SA_EV_Date))
  colnames(SA_cov)<-c("id", "date")
  fwrite(SA_cov, paste0(output_folder,"Spont_Abort.csv"))
  
    
#################################################################################
  # STILL BIRTH
  
  # Aarhus uses only SURVEY_OB
  # so_source_column= "LEVENDE_ELLER_DOEDFOEDT"  AND so_source_value= "DOEDFOEDT" 
  
 
  
  my_rows<-which(SURV_OB$so_source_column=="LEVENDE_ELLER_DOEDFOEDT" & SURV_OB$so_source_value=="DOEDFOEDT")

  
  SB_SO_ID<-SURV_OB$person_id[my_rows]
  SB_SO_Date<-SURV_OB$so_date[my_rows]

  SB_cov<-as.data.frame(cbind(SB_SO_ID,SB_SO_Date))
  colnames(SB_cov)<-c("id", "date")
  fwrite(SB_cov, paste0(output_folder,"Still_Birth.csv"))
  
  
  #################################################################################
  # PREECLAMPSIA
  
  # Aarhus USES events only
  
  my_rows<-which(Reduce(`|`, lapply("P_PREECLAMP_AESI", startsWith, x = as.character(all_codes$full_name))))
  
  PREECLAMP_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(PREECLAMP_codes, startsWith, x = as.character(EVENTS$event_code))))
  PREECLAMP_ID<-(EVENTS$person_id[my_rows])
  PREECLAMP_Date<- (EVENTS$start_date_record[my_rows])
  
  
  PREECLAMP_cov<-as.data.frame(cbind(PREECLAMP_ID,PREECLAMP_Date))
  colnames(PREECLAMP_cov)<-c("id", "date")
  fwrite(PREECLAMP_cov, paste0(output_folder,"Preeclampsia.csv"))

  #################################################################################
  # TOPFA
  
  # Aarhus USES events only
  
  TOPFA_names<-c("P_SUSPFETANOM_AESI","P_ELECTTERM_AESI" )
  
  my_rows<-which(Reduce(`|`, lapply(TOPFA_names, startsWith, x = as.character(all_codes$full_name))))
  
  TOPFA_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(TOPFA_codes, startsWith, x = as.character(EVENTS$event_code))))
  TOPFA_ID<-(EVENTS$person_id[my_rows])
  TOPFA_Date<- (EVENTS$start_date_record[my_rows])
  
  
  TOPFA_cov<-as.data.frame(cbind(TOPFA_ID,TOPFA_Date))
  colnames(TOPFA_cov)<-c("id", "date")
  fwrite(TOPFA_cov, paste0(output_folder,"TOPFA.csv"))
  
  #################################################################################
  # PRE-TERM BIRTH
  # ALL DAPs use pregnancy algorithm output (LB &gest_age<37 weeks)
  # Aarhus uses SURVEY_OB so_source_column="GESTATIONSALDER_DAGE" AND so_source_value=<259
  # Aarhus DOES NOT use EVENTS 
  
  df_preg<- fread(paste0(cohort_folder, my_preg_data[i]))
  
  df_preg$gest_weeks<-(df_preg$pregnancy_end_date-df_preg$pregnancy_start_date)/7
  
  PRETERM_alg_ID<-df_preg$person_id[(df_preg$gest_weeks<37)&(df_preg$type_of_pregnancy_end=="LB")]
  PRETERM_alg_Date<-df_preg$pregnancy_end_date[(df_preg$gest_weeks<37)&(df_preg$type_of_pregnancy_end=="LB")]
  
  PRETERM_SO_ID<-SURV_OB$person_id[SURV_OB$so_source_column=="GESTATIONSALDER_DAGE" & SURV_OB$so_source_value<=259]
  PRETERM_SO_Date<-SURV_OB$so_date[SURV_OB$so_source_column=="GESTATIONSALDER_DAGE" & SURV_OB$so_source_value<=259]
  
  PRETERM_ID<-c(PRETERM_alg_ID, PRETERM_SO_ID)
  PRETERM_DATE<-c(PRETERM_alg_Date, PRETERM_SO_DATE)
  
  PRETERM_cov<-as.data.frame(cbind(PRETERM_ID,PRETERM_Date))
  colnames(PRETERM_cov)<-c("id", "date")
  fwrite(PRETERM_cov, paste0(output_folder,"PRETERM.csv"))
   
  
#####################################################################

#MATERNAL DEATH


# Aarhus uses only PERSONS and delivery date


dead_PERSONS<-PERSONS[is.na(PERSONS$year_of_death)==F,]
dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]<-paste0(0,(dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]))
dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]<-paste0(0,(dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]))
dead_PERSONS$death_date<-paste0(dead_PERSONS$year_of_death, dead_PERSONS$month_of_death,dead_PERSONS$day_of_death)
dead_PERSONS$death_date<-as.numeric(as.Date(dead_PERSONS$death_date, format="%Y%m%d"))

dead_mother<-my_PREG[my_PREG$person_id%in%dead_PERSONS$person_id]

maternal_death<-dead_PERSONS[between(dead_PERSONS$death_date, dead_mother$pregnancy_start_date, (dead_mother$pregnancy_end_date)+42),]
maternal_death_pers_Date<-maternal_death$death_date
maternal_death_pers_ID<-maternal_death$person_id

maternal_death_id<-c( maternal_death_pers_ID)
maternal_death_date<-c( maternal_death_pers_Date)
maternal_death_outcome<-as.data.frame(cbind(maternal_death_id, maternal_death_date))
colnames(maternal_death_outcome)<-c("id", "date")
fwrite(maternal_death_outcome, paste0(output_folder,"maternal_death.csv"))}
