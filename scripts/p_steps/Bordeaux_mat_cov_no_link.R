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
  
  EVENTS<-IMPORT_PATTERN(pat="EVENTS", dir=cohort_folder)
  MED_OB<-IMPORT_PATTERN(pat="MEDICAL_OB", dir=cohort_folder)
  SURV_OB<-IMPORT_PATTERN(pat="SURVEY", dir=cohort_folder)
  MED<-IMPORT_PATTERN(pat="MEDICINES", dir=cohort_folder)
  PROC<-IMPORT_PATTERN(pat="PROCEDURE", dir=cohort_folder)
  PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=cohort_folder)
  my_PREG<- IMPORT_PATTERN(pat=my_preg_data[i], dir=cohort_folder)
  
  MED$drug_date<-MED$date_dispensing
  
  #################################################################################
  # GEST_DIAB
  # Bordeaux ALSO uses PROCEDURES
  # origin_of_procedure = "BIOLOGIE" AND procedure_code="0412"
  
  GEST_DIAB_names<-c("P_GESTDIAB_AESI")
  my_rows<-which(Reduce(`|`, lapply(GEST_DIAB_names, startsWith, x = as.character(all_codes$full_name))))
  
  GEST_DIAB_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(GEST_DIAB_codes, startsWith, x = as.character(EVENTS$event_code))))
  GEST_DIAB_EV_ID<-(EVENTS$person_id[my_rows])
  GEST_DIAB_EV_Date<- (EVENTS$start_date_record[my_rows])
 
  my_rows<-which(PROC$origin_of_procedure=="BIOLOGIE"& PROC$procedure_code=="0412")
  GEST_DIAB_PROC_ID<-PROC$person_id[my_rows]
  GEST_DIAB_PROC_Date<-PROC$procedure_date[my_rows]
  
  GEST_DIAB_ID<-c(GEST_DIAB_EV_ID, GEST_DIAB_PROC_ID)
  GEST_DIAB_Date<-c(GEST_DIAB_EV_Date, GEST_DIAB_PROC_Date)
  
  GEST_DIAB_cov<-as.data.frame(cbind(GEST_DIAB_ID, GEST_DIAB_Date))
  colnames(GEST_DIAB_cov)<-c("id", "date")
  fwrite(GEST_DIAB_cov, paste0(output_folder,"gest_diab.csv"))
  
  #################################################################################
  # CAESARIAN
  
  # BORDEAUX uses EVENTS and PROC
  # origin_of_procedure = "PROCEDURE" AND procedure_code in ("JQGA002" "JQGA003" "JQGA004" "JQGA003")
  
  
  my_rows<-which(Reduce(`|`, lapply("TP_CESAREA_COV", startsWith, x = as.character(all_codes$full_name))))
  
  CESAREA_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(CESAREA_codes, startsWith, x = as.character(EVENTS$event_code))))
  CESAREA_EV_ID<-(EVENTS$person_id[my_rows])
  CESAREA_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  my_rows<-which(PROC$origin_of_procedure=="PROCEDURE"& PROC$procedure_code%in%c("JQGA002", "JQGA002", "JQGA004", "JQGA005"))
  CESAREA_PROC_ID<-PROC$person_id[my_rows]
  CESAREA_PROC_Date<-PROC$procedure_date[my_rows]
  
  CESAREA_ID<-c(CESAREA_EV_ID, CESAREA_PROC_ID)
  CESAREA_Date<-c(CESAREA_EV_Date, CESAREA_PROC_Date)
  
  CESAREA_cov<-as.data.frame(cbind(CESAREA_ID, CESAREA_Date))
  colnames(CESAREA_cov)<-c("id", "date")
  fwrite(CESAREA_cov, paste0(output_folder,"CESAREA.csv"))

 #################################################################################
  # SPONTANEOUS ABORTION
  
  # BORDEAUX uses EVENTS, AND MED_OB and SURV_OB
  # mo_source_value = "AGE_GES" AND mo_unit = "weeks" AND mo_source_column < 22
  # so_source_column="DIAG" AND so_source_value = "PRE5"
  
  my_rows<-which(Reduce(`|`, lapply("P_SPONTABO_AESI", startsWith, x = as.character(all_codes$full_name))))
  
  SA_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(SA_codes, startsWith, x = as.character(EVENTS$event_code))))
  SA_EV_ID<-(EVENTS$person_id[my_rows])
  SA_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  my_rows<-which(SURV_OB$so_source_column=="DIAG" & SURV_OB$so_source_value=="PRE5")
  
  SA_SO_ID<-SURV_OB$person_id[my_rows]
  SA_SO_Date<-SURV_OB$so_date[my_rows]
  
  my_rows<-which(MED_OB$mo_source_value=="AGE_GES"&MED_OB$mo_unit="weeks"&MED_OB$mo_source_column<22)
  
  SA_MO_ID<-MED_OB$person_id[my_rows]
  SA_MO_Date<-MED_OB$mo_date[my_rows]
  
  SA_ID<-c(SA_EV_ID,SA_SO_ID, SA_MO_ID)
  SA_Date<-C(SA_EV_Date, SA_SO_Date, SA_MO_Date)
  
  SA_cov<-as.data.frame(cbind(SA_ID, SA_Date))
  colnames(SA_cov)<-c("id", "date")
  fwrite(SA_cov, paste0(output_folder,"Spont_Abort.csv"))
  
    
#################################################################################
  # STILL BIRTH
  
  # Bordeaux uses Events, survey_ob and med_ob
  # mo_source_value = "AGE_GES" AND mo_unit = "weeks" AND mo_source_column >= 22
  # so_cource_column="DIAG" AND so_source_value = "PRE10"
  
  my_rows<-which(Reduce(`|`, lapply("P_STILLBIRTH_AESI", startsWith, x = as.character(all_codes$full_name))))
  
  SB_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(SB_codes, startsWith, x = as.character(EVENTS$event_code))))
  SB_EV_ID<-(EVENTS$person_id[my_rows])
  SB_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  
  my_rows<-which(SURV_OB$so_source_column=="DIAG" & SURV_OB$so_source_value=="PRE10")

  
  SB_SO_ID<-SURV_OB$person_id[my_rows]
  SB_SO_Date<-SURV_OB$so_date[my_rows]

  my_rows<-which(MED_OB$mo_source_value=="AGE_GES"&MED_OB$mo_unit="weeks"&MED_OB$mo_source_column>=22)
  
  SB_MO_ID<-MED_OB$person_id[my_rows]
  SB_MO_Date<-MED_OB$mo_date[my_rows]
  
  SB_ID<-c(SB_EV_ID, SB_SO_ID, SB_MO_ID)
  SB_Date<-c(SB_EV_Date, SB_SO_Date, SB_MO_Date)
  
  SB_cov<-as.data.frame(cbind(SB_ID,SB_Date))
  colnames(SB_cov)<-c("id", "date")
  fwrite(SB_cov, paste0(output_folder,"Still_Birth.csv"))
  
  
  #################################################################################
  # PREECLAMPSIA
  
  # BORDEAUX USES events only
  
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
  
  # Bordeaux USES events and SO and PROCEDURE
  # so_cource_column="DIAG" AND so_source_value = "PRE2"
  # "Induced termination : origin_of_procedure = ""PROCEDURE"" and procedure_code = ""JNJD001"" or ""JNJD002"" or ""JNJP001""
  # Medicated VTP: origin_of_procedure = ""PRESTATION_COUT"" and procedure_code = ""2415"" or ""2416""  or ""2420"" or ""2422"" or ""3329"""
  
  TOPFA_names<-c("P_SUSPFETANOM_AESI","P_ELECTTERM_AESI" )
  
  my_rows<-which(Reduce(`|`, lapply(TOPFA_names, startsWith, x = as.character(all_codes$full_name))))
  
  TOPFA_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(TOPFA_codes, startsWith, x = as.character(EVENTS$event_code))))
  TOPFA_EV_ID<-(EVENTS$person_id[my_rows])
  TOPFA_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  my_rows<-which(SURV_OB$so_source_column=="DIAG" & SURV_OB$so_source_value=="PRE2")
  

 TOPFA_SO_ID<-SURV_OB$person_id[my_rows]
 TOPFA_SO_Date<-SURV_OB$so_date[my_rows]
  
  my_rows1<-which(PROC$origin_of_procedure=="PROCEDURE" & PROC$procedure_code%in%c("JNJD001", "JNJD002", "JNJP001"))
  my_rows2<-which(PROC$origin_of_procedure=="PRESTATION_COUT" & PROC$procedure_code%in%c("2415","2416", "2420","2422","3329"))
  
  my_rows<-unique(c(my_rows1, my_rows2))
  
 TOPFA_PROC_ID<-PROC$person_id[my_rows]
 TOPFA_PROC_Date<-PROC$procedure_date[my_rows]
  
 TOPFA_ID<-c(TOPFA_EV_ID,TOPFA_SO_ID,TOPFA_PROC_ID)
 TOPFA_Date<-c(TOPFA_EV_Date,TOPFA_SO_Date,TOPFA_PROC_Date)
  
  
  TOPFA_cov<-as.data.frame(cbind(TOPFA_ID,TOPFA_Date))
  colnames(TOPFA_cov)<-c("id", "date")
  fwrite(TOPFA_cov, paste0(output_folder,"TOPFA.csv"))
  
  #################################################################################
  # PRE-TERM BIRTH
  # ALL DAPs use pregnancy algorithm output (LB &gest_age<37 weeks)
  # BORDEAUX uses EVENTS, MED_OB and PROC
  # mo_source_value = "AGE_GES" AND mo_unit = "weeks" AND mo_source_column < 37 
  # HELP "Live birth : origin_of_procedure = ""PROCEDURE"" and procedure_code in (""JQGD002"" ""JQGD003"" ""JQGD004"" ""JQGD005"" ""JQGD007"" ""JQGD008"" ""JQGD010"" ""JQGD012"" ""JQGD013"" ""JQGA002"" ""JQGA003"" JQGA004"" ""JQGA005"")"
  
  my_rows<-which(Reduce(`|`, lapply("P_PRETERMBIRTH_AESI", startsWith, x = as.character(all_codes$full_name))))
  
  PRETERM_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(PRETERM_codes, startsWith, x = as.character(EVENTS$event_code))))
  PRETERM_EV_ID<-(EVENTS$person_id[my_rows])
  PRETERM_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  df_preg<- fread(paste0(cohort_folder, my_preg_data[i]))
  
  df_preg$gest_weeks<-(df_preg$pregnancy_end_date-df_preg$pregnancy_start_date)/7
  
  PRETERM_alg_ID<-df_preg$person_id[(df_preg$gest_weeks<37)&(df_preg$type_of_pregnancy_end=="LB")]
  PRETERM_alg_Date<-df_preg$pregnancy_end_date[(df_preg$gest_weeks<37)&(df_preg$type_of_pregnancy_end=="LB")]
  
  PRETERM_MO_ID<-MED_OB$person_id[MED_OB$mo_source_value=="AGE_GES" & MED_OB$mo_unit="weeks" & MED_OB$mo_source_column<37]
  PRETERM_MO_Date<-MED_OB$person_id[MED_OB$mo_source_value=="AGE_GES" & MED_OB$mo_unit="weeks" & MED_OB$mo_source_column<37]
  
  PRETERM_ID<-c(PRETERM_EV_ID,PRETERM_alg_ID, PRETERM_MO_ID)
  PRETERM_DATE<-c(PRETERM_EV_Date, PRETERM_alg_Date, PRETERM_MO_DATE)
  
  PRETERM_cov<-as.data.frame(cbind(PRETERM_ID,PRETERM_Date))
  colnames(PRETERM_cov)<-c("id", "date")
  fwrite(PRETERM_cov, paste0(output_folder,"PRETERM.csv"))
   
  
#####################################################################

#MATERNAL DEATH

# Bordeaux uses Events, Persons and SURV_OB
# so_source_table='DECES', death date is so_date AND so_date is before end_date_pregnancy OR less than 42 days after end_date_pregnancy
  
  maternal_death_names<-"P_MATERNALDEATH_AESI"
  my_rows<-which(Reduce(`|`, lapply(maternal_death_names, startsWith, x = as.character(all_codes$full_name))))
  
  maternal_death_codes<- unique(all_codes$code[my_rows])
  
  my_rows<-which(Reduce(`|`, lapply(maternal_death_codes, startsWith, x = as.character(EVENTS$event_code))))
  maternal_death_EV_ID<-(EVENTS$person_id[my_rows])
  maternal_death_EV_Date<- (EVENTS$start_date_record[my_rows])
  
  # maternal_death_SO_ID<- SURV_OB$person_id[SURV_OB$so_source_table=="DECES"]
  # maternal_death_SO_Date<- SURV_OB$so_date[SURV_OB$so_source_table=="DECES"] 
  # 
  
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
