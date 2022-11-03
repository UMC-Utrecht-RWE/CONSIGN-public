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
  
  
  my_event_name<-"TP_CESAREA_COV"
  
  CESAREA_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  CreateConceptDatasets(codesheet = CESAREA_codelist, fil=EVENTS, path = maternal_covariates_events)
  
  CESAREA_EV<-readRDS(paste0(maternal_covariates_events,my_event_name,".rds"))
  CESAREA_EV_ID<-(GEST_DIAB_EV$person_id)
  CESAREA_EV_Date<- (GEST_DIAB_EV$start_date_record)
  
  
  CESAREA_cov<-as.data.frame(cbind(CESAREA_EV_ID, CESAREA_EV_Date))
  colnames(CESAREA_cov)<-c("id", "date")
  fwrite(CESAREA_cov, paste0(output_folder,"CESAREA.csv"))

 #################################################################################
  # SPONTANEOUS ABORTION
  
  # KI USES events and pregnancy algorithm outputs
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
  
  # KI USES events and pregnancy algorithm output
  
  my_event_name<-"P_STILLBIRTH_AESI"
  
  SB_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  CreateConceptDatasets(codesheet = SB_codelist, fil=EVENTS, path = maternal_covariates_events)
  
  SB_EV<-readRDS(paste0(maternal_covariates_events,my_event_name,".rds"))
  SB_EV_ID<-(SB_EV$person_id)
  SB_EV_Date<- (SB_EV$start_date_record)
  
  
  
  
  SB_alg_ID<-df_preg$person_id[df_preg$type_of_pregnancy_end=="SB"]
  SB_alg_Date<-df_preg$pregnancy_end_date[df_preg$type_of_pregnancy_end=="SB"]
  
  SB_ID<-c(SB_EV_ID, SB_alg_ID)
  SB_Date<-c(SB_EV_Date, SB_alg_Date)
  
  SB_cov<-as.data.frame(cbind(SB_ID,SB_Date))
  colnames(SB_cov)<-c("id", "date")
  fwrite(SB_cov, paste0(output_folder,"Still_Birth.csv"))
  
  
  #################################################################################
  # PREECLAMPSIA
  
  # KI USES events only
  
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
  
  # KI USES events only
  
  # TOPFA_names<-c("P_SUSPFETANOM_AESI","P_ELECTTERM_AESI" )
  # 
  # my_rows<-which(Reduce(`|`, lapply(TOPFA_names, startsWith, x = as.character(all_codes$full_name))))
  # 
  # TOPFA_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))
  # 
  # my_rows<-which(Reduce(`|`, lapply(TOPFA_codes, startsWith, x = as.character(EVENTS$event_code))))
  # TOPFA_ID<-(EVENTS$person_id[my_rows])
  # TOPFA_Date<- (EVENTS$start_date_record[my_rows])
  # 
  
  TOPFA_cov<-as.data.frame(cbind(NA,NA))
  colnames(TOPFA_cov)<-c("id", "date")
  fwrite(TOPFA_cov, paste0(output_folder,"TOPFA.csv"))
  
  #################################################################################
  # PRE-TERM BIRTH
  
  # KI  pregnancy algorithm output
  
  
  
  
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


# same for all DAPs : EVENTS codes tagged as P_MATERNALDEATH_AESI in VAC4EU all_codes

maternal_death_names<-"P_MATERNALDEATH_AESI"
my_rows<-which(Reduce(`|`, lapply(maternal_death_names, startsWith, x = as.character(all_codes$full_name))))

maternal_death_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(maternal_death_codes, startsWith, x = as.character(EVENTS$event_code))))
maternal_death_EV_ID<-(EVENTS$person_id[my_rows])
maternal_death_EV_Date<- (EVENTS$start_date_record[my_rows])

dead_PERSONS<-PERSONS[is.na(PERSONS$year_of_death)==F,]
dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]<-paste0(0,(dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]))
dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]<-paste0(0,(dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]))
dead_PERSONS$death_date<-paste0(dead_PERSONS$year_of_death, dead_PERSONS$month_of_death,dead_PERSONS$day_of_death)
dead_PERSONS$death_date<-as.numeric(as.Date(dead_PERSONS$death_date, format="%Y%m%d"))

dead_mother<-my_PREG[my_PREG$person_id%in%dead_PERSONS$person_id]

maternal_death<-dead_PERSONS[between(dead_PERSONS$death_date, dead_mother$pregnancy_start_date, (dead_mother$pregnancy_end_date)+42),]
maternal_death_pers_Date<-maternal_death$death_date
maternal_death_pers_ID<-maternal_death$person_id

maternal_death_id<-c(maternal_death_EV_ID, maternal_death_pers_ID)
maternal_death_date<-c(maternal_death_EV_Date, maternal_death_pers_Date)
maternal_death_outcome<-as.data.frame(cbind(maternal_death_id, maternal_death_date))
colnames(maternal_death_outcome)<-c("id", "date")
fwrite(maternal_death_outcome, paste0(output_folder,"maternal_death.csv"))}

