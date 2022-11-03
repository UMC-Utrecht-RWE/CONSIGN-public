#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#17/10/2022

# due to wide variety of DAP specific criteria, maternal covariates will be coded separately per DAP

preg_cohort_folders<-list(hist_preg_folder,preg_match_folder, cases_match_folder)
output_folders<-list(output_mat_cov_hist, output_mat_cov_pan_neg, output_mat_cov_pan_pos)

my_preg_data<-c("my_PREG.csv", "preg_trim.csv", "preg_trim.csv")

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
  
  my_event_name<-"P_GESTDIAB_AESI"
  
  GEST_DIAB_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  CreateConceptDatasets(codesheet = GEST_DIAB_codelist, fil=EVENTS, path = maternal_covariates_events)
  
  GEST_DIAB_EV<-readRDS(paste0(maternal_covariates_events,my_event_name,".rds"))
  GEST_DIAB_EV_ID<-(GEST_DIAB_EV$person_id)
  GEST_DIAB_EV_Date<- (GEST_DIAB_EV$start_date_record)
  
 
  GEST_DIAB_cov<-as.data.frame(cbind(GEST_DIAB_EV_ID, GEST_DIAB_EV_Date))
  colnames(GEST_DIAB_cov)<-c("id", "date")
  fwrite(GEST_DIAB_cov, paste0(output_folder,"gest_diab.csv"))
  
  #################################################################################
  # CAESARIAN
  
  # Aarhus uses EVENTS and SURVEY_OB 
  # so_source_column= "KEJSERSNIT_MODERSOENSKE"  or so_source_column="Markoer_kejsersnit"  AND  so_source_value= procedure code/non NA
  
  my_event_name<-"TP_CESAREA_COV"
  
  CESAREA_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  CreateConceptDatasets(codesheet = CESAREA_codelist, fil=EVENTS, path = maternal_covariates_events)
  
  CESAREA_EV<-readRDS(paste0(maternal_covariates_events,my_event_name,".rds"))
  CESAREA_EV_ID<-(GEST_DIAB_EV$person_id)
  CESAREA_EV_Date<- (GEST_DIAB_EV$start_date_record)
  
  
  my_rows<-which(SURV_OB$so_source_column%in%c("KEJSERSNIT_MODERSOENSKE", "MARKOER_KEJSERSNIT")& (is.na(SURV_OB$so_source_value)==F))
  CESAREA_SO_ID<-SO$person_id[my_rows]
  CESAREA_SO_Date<-SO$so_date[my_rows]
  
  CESAREA_ID<-c(CESAREA_EV_ID, CESAREA_SO_ID)
  CESAREA_Date<-c(CESAREA_EV_Date, CESAREA_SO_Date)
  
  CESAREA_cov<-as.data.frame(cbind(CESAREA_ID, CESAREA_Date))
  colnames(CESAREA_cov)<-c("id", "date")
  fwrite(CESAREA_cov, paste0(output_folder,"CESAREA.csv"))

 #################################################################################
  # SPONTANEOUS ABORTION
  
  # Aarhus USES events and output of the pregnancy algorithm 
  my_event_name<-"P_SPONTABO_AESI"
  
  SPONTABO_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  CreateConceptDatasets(codesheet = SPONTABO_codelist, fil=EVENTS, path = maternal_covariates_events)
  
  SPONTABO_EV<-readRDS(paste0(maternal_covariates_events,my_event_name,".rds"))
  SA_EV_ID<-(SPONTABO_EV$person_id)
  SA_EV_Date<- (SPONTABO_EV$start_date_record)
  
  
  SA_alg_ID<-df_preg$person_id[df_preg$type_of_pregnancy_end=="SA"]
  SA_alg_Date<-df_preg$pregnancy_end_date[df_preg$type_of_pregnancy_end=="SA"]
  
  SA_ID<-c(SA_EV_ID, SA_alg_ID)
  SA_Date<-c(SA_EV_Date, SA_alg_Date)
  
  SA_cov<-as.data.frame(cbind(SA_ID, SA_Date))
  colnames(SA_cov)<-c("id", "date")
  fwrite(SA_cov, paste0(output_folder,"Spont_Abort.csv"))
  
    
#################################################################################
  # STILL BIRTH
  
  # Aarhus uses only SURVEY_OB and preg alg
  # so_source_column= "LEVENDE_ELLER_DOEDFOEDT"  AND so_source_value= "DOEDFOEDT" 
  
 
  
  my_rows<-which(SURV_OB$so_source_column=="LEVENDE_ELLER_DOEDFOEDT" & SURV_OB$so_source_value=="DOEDFOEDT")

  
  SB_SO_ID<-SURV_OB$person_id[my_rows]
  SB_SO_Date<-SURV_OB$so_date[my_rows]
  
  
  SB_alg_ID<-df_preg$person_id[df_preg$type_of_pregnancy_end=="SB"]
  SB_alg_Date<-df_preg$pregnancy_end_date[df_preg$type_of_pregnancy_end=="SB"]
  
  SB_ID<-c(SB_SO_ID, SB_alg_ID)
  SB_Date<-c(SB_SO_Date, SB_alg_Date)
  
  SB_cov<-as.data.frame(cbind(SB_ID,SB_Date))
  colnames(SB_cov)<-c("id", "date")
  fwrite(SB_cov, paste0(output_folder,"Still_Birth.csv"))
  
  
  #################################################################################
  # PREECLAMPSIA
  
  # Aarhus USES events only
  
  my_event_name<-"P_PREECLAMP_AESI"
  
  PREECLAMP_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  CreateConceptDatasets(codesheet = PREECLAMP_codelist, fil=EVENTS, path = maternal_covariates_events)
  
  PREECLAMP_EV<-readRDS(paste0(maternal_covariates_events,my_event_name,".rds"))
  PREECLAMP_EV_ID<-(PREECLAMP_EV$person_id)
  PREECLAMP_EV_Date<- (PREECLAMP_EV$start_date_record)
  
  PREECLAMP_cov<-as.data.frame(cbind(PREECLAMP_EV_ID,PREECLAMP_EV_Date))
  colnames(PREECLAMP_cov)<-c("id", "date")
  fwrite(PREECLAMP_cov, paste0(output_folder,"Preeclampsia.csv"))

  #################################################################################
  # TOPFA
  
  # Aarhus USES events only
  # 
  # TOPFA_names<-c("P_SUSPFETANOM_AESI","P_ELECTTERM_AESI" )
  # 
  # my_rows<-which(Reduce(`|`, lapply(TOPFA_names, startsWith, x = as.character(all_codes$full_name))))
  # 
  # TOPFA_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))
  # 
  # my_rows<-which(Reduce(`|`, lapply(TOPFA_codes, startsWith, x = as.character(EVENTS$event_code))))
  # TOPFA_ID<-(EVENTS$person_id[my_rows])
  # TOPFA_Date<- (EVENTS$start_date_record[my_rows])
  
  
  TOPFA_cov<-as.data.frame(cbind(NA,NA))
  colnames(TOPFA_cov)<-c("id", "date")
  fwrite(TOPFA_cov, paste0(output_folder,"TOPFA.csv"))
  
  #################################################################################
  # PRE-TERM BIRTH
  # ALL DAPs use pregnancy algorithm output (LB &gest_age<37 weeks)
  # Aarhus uses SURVEY_OB so_source_column="GESTATIONSALDER_DAGE" AND so_source_value=<259
  # Aarhus DOES NOT use EVENTS 
  
  
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

