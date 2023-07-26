#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#23/10/2022

# due to wide variety of DAP specific criteria, NEONATAL covariates will be coded separately per DAP

neonate_cohort_folders<-list(case_neonate_folder,control_neonate_folder, historical_neonate_folder)
output_folders<-list(output_neonates_case, output_neonates_control, output_neonates_hist)

# my_neonate_data<-c("cov_pos_preg.csv", "cov_neg_preg.csv", "my_PREG.csv")

all_codes<-fread(paste0(projectFolder,"/codelist_CONSIGN.csv"))
all_codes<-all_codes[tags=="narrow",]

# only events within 1 year before covid+ pregnancy start date
# filter source data events everything before Jan 1 2019 (too old to be within covid preg window)

for(i in 1:length(neonate_cohort_folders)){
  
  cohort_folder<-unlist(neonate_cohort_folders[i])
  output_folder<-unlist(output_folders[i])
  
  EVENTS<-IMPORT_PATTERN(pat="EVENTS", dir=cohort_folder)
  MED_OB<-IMPORT_PATTERN(pat="MED_OB", dir=cohort_folder)
  SURV_OB<-IMPORT_PATTERN(pat="SURVEY", dir=cohort_folder)
  MED<-IMPORT_PATTERN(pat="MEDICINES", dir=cohort_folder)
  PROC<-IMPORT_PATTERN(pat="PROCEDURE", dir=cohort_folder)
  PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=cohort_folder)
  my_neonate<- IMPORT_PATTERN(pat="neonate", dir=cohort_folder)
  
  
  MED$drug_date<-MED$date_dispensing
  MED$drug_date<-as.numeric(as.Date(MED$drug_date, format="%Y%m%d"))
  
  #################################################################################
  # SGA or FGR
  
  my_event_name<-c("P_FGR_AESI")
  
  FGR_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  
  CreateConceptDatasets(codesheet = FGR_codelist, fil=EVENTS, path = output_folder)
  
  FGR_EV<-readRDS(paste0(output_folder,"FGR.rds"))
  FGR_EV_ID<-(FGR_EV$person_id)
  FGR_EV_Date<- (FGR_EV$start_date_record)
  
  # so_source_colum  = "ACCR_FET" AND so_source_value = 1 
  
  FGR_SO_ID<-SURV_OB$person_id[((SURV_OB$so_source_column=="ACCR_FET")&(SURV_OB$so_source_value==1))]
  FGR_SO_Date<-SURV_OB$so_date[((SURV_OB$so_source_column=="ACCR_FET")&(SURV_OB$so_source_value==1))]
 
  FGR_cov<-as.data.frame(cbind(c(FGR_EV_ID, FGR_SO_ID), c(FGR_EV_Date, FGR_SO_Date)))
  colnames(FGR_cov)<-c("id", "date")
  fwrite(FGR_cov, paste0(output_folder,"SGA_FGR.csv"))
  
  #################################################################################
  # LBW
  # events and survey observations
  # so_source_table= "MDR" AND so_source_column == "peso"  AND so_source_value= <2500
  
  my_event_name<-c("P_LBW_AESI")
  
  LBW_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  
  CreateConceptDatasets(codesheet = LBW_codelist, fil=EVENTS, path = output_folder)
  
  LBW_EV<-readRDS(paste0(output_folder,"LBW.rds"))
  LBW_EV_ID<-(LBW_EV$person_id)
  LBW_EV_Date<- (LBW_EV$start_date_record)
  
  # so_source_table = "CAP2" AND so_source_column == "peso" AND so_source_value= <2500
  
  LBW_SO_ID<-SURV_OB$person_id[((SURV_OB$so_source_table=="CAP2")&(SURV_OB$so_source_column=="PESO")&(SURV_OB$so_source_value<=2500))]
  LBW_SO_Date<-SURV_OB$so_date[((SURV_OB$so_source_table=="CAP2")&(SURV_OB$so_source_column=="PESO")&(SURV_OB$so_source_value<=2500))]
  
  LBW_cov<-as.data.frame(cbind(c(LBW_EV_ID, LBW_SO_ID), c(LBW_EV_Date, LBW_SO_Date)))
  colnames(LBW_cov)<-c("id", "date")
  fwrite(LBW_cov, paste0(output_folder,"LBW.csv"))
  
#####################################################################
# low apgar

  my_event_name<-c("P_APGARLOW_AESI")
  
  APGARLOW_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  
  CreateConceptDatasets(codesheet = APGARLOW_codelist, fil=EVENTS, path = output_folder)
  
  APGARLOW_EV<-readRDS(paste0(output_folder,"APGARLOW.rds"))
  APGARLOW_EV_ID<-(APGARLOW_EV$person_id)
  APGARLOW_EV_Date<- (APGARLOW_EV$start_date_record)
  
  # so_source_column  = "APGAR" AND so_source_value= "0" or "1" or "2" or "3"
  
  APGARLOW_SO_ID<-SURV_OB$person_id[((SURV_OB$so_source_column=="APGAR")&(SURV_OB$so_source_value%in%c(0,1,2,3,4,5,6)))]
  APGARLOW_SO_Date<-SURV_OB$so_date[((SURV_OB$so_source_column=="APGAR")&(SURV_OB$so_source_value%in%c(0,1,2,3,4,5,6)))]
  
  APGARLOW_cov<-as.data.frame(cbind(c(APGARLOW_EV_ID, APGARLOW_SO_ID), c(APGARLOW_EV_Date, APGARLOW_SO_Date)))
  colnames(APGARLOW_cov)<-c("id", "date")
  fwrite(APGARLOW_cov, paste0(output_folder,"APGARLOW.csv"))

##########################################################################    
  # microcephaly
  
  my_event_name<-c("P_MICROCEPHALY_AESI")
  
  MICROCEPHALY_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  
  CreateConceptDatasets(codesheet = MICROCEPHALY_codelist, fil=EVENTS, path = output_folder)
  
  MICROCEPHALY_EV<-readRDS(paste0(output_folder,"MICROCEPHALY.rds"))
  MICROCEPHALY_EV_ID<-(MICROCEPHALY_EV$person_id)
  MICROCEPHALY_EV_Date<- (MICROCEPHALY_EV$start_date_record)
  
  MICROCEPHALY_cov<-as.data.frame(cbind(MICROCEPHALY_EV_ID, MICROCEPHALY_EV_Date))
  colnames(MICROCEPHALY_cov)<-c("id", "date")
  fwrite(MICROCEPHALY_cov, paste0(output_folder,"MICROCEPHALY.csv"))
  
###################################################################################  
  
  # major congenital anomoly P_MAJORCA_AESI
  
  my_event_name<-c("P_CAABD_AESI")
  
  MAJORCA_codelist<-all_codes[all_codes$event_match_name==my_event_name,]
  
  CreateConceptDatasets(codesheet = MAJORCA_codelist, fil=EVENTS, path = output_folder)
  
  MAJORCA_EV<-readRDS(paste0(output_folder,"CAABD.rds"))
  MAJORCA_EV_ID<-(MAJORCA_EV$person_id)
  MAJORCA_EV_Date<- (MAJORCA_EV$start_date_record)
  
  MAJORCA_cov<-as.data.frame(cbind(MAJORCA_EV_ID, MAJORCA_EV_Date))
  colnames(MAJORCA_cov)<-c("id", "date")
  fwrite(MAJORCA_cov, paste0(output_folder,"MAJORCA.csv"))
  
#NEONATAL DEATH
  # PERSONS$year_of_death[1:20]<-PERSONS$year_of_birth[1:20]
  # PERSONS$month_of_death[1:20]<-((as.numeric(PERSONS$month_of_birth[1:20]))+(sample(c(0,1),20, replace=T)))
  # PERSONS$day_of_death[1:20]<-30
  
dead_PERSONS<-PERSONS[is.na(PERSONS$year_of_death)==F,]
dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]<-paste0(0,(dead_PERSONS$day_of_death[nchar(dead_PERSONS$day_of_death)==1]))
dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]<-paste0(0,(dead_PERSONS$month_of_death[nchar(dead_PERSONS$month_of_death)==1]))
dead_PERSONS$death_date<-paste0(dead_PERSONS$year_of_death, dead_PERSONS$month_of_death,dead_PERSONS$day_of_death)
dead_PERSONS$death_date<-as.numeric(as.Date(dead_PERSONS$death_date, format="%Y%m%d"))

dead_PERSONS$day_of_birth[nchar(dead_PERSONS$day_of_birth)==1]<-paste0(0,(dead_PERSONS$day_of_birth[nchar(dead_PERSONS$day_of_birth)==1]))
dead_PERSONS$month_of_birth[nchar(dead_PERSONS$month_of_birth)==1]<-paste0(0,(dead_PERSONS$month_of_birth[nchar(dead_PERSONS$month_of_birth)==1]))
dead_PERSONS$birth_date<-paste0(dead_PERSONS$year_of_birth, dead_PERSONS$month_of_birth,dead_PERSONS$day_of_birth)
dead_PERSONS$birth_date<-as.numeric(as.Date(dead_PERSONS$birth_date, format="%Y%m%d"))

dead_PERSONS$days_alive<-dead_PERSONS$death_date-dead_PERSONS$birth_date

dead_PERSONS$days_alive

NEONATAL_death<-dead_PERSONS[dead_PERSONS$days_alive<=28,]
NEONATAL_death_pers_Date<-NEONATAL_death$death_date
NEONATAL_death_pers_ID<-NEONATAL_death$person_id

NEONATAL_death_id<-c( NEONATAL_death_pers_ID)
NEONATAL_death_date<-c( NEONATAL_death_pers_Date)
NEONATAL_death_outcome<-as.data.frame(cbind(NEONATAL_death_id, NEONATAL_death_date))
colnames(NEONATAL_death_outcome)<-c("id", "date")
fwrite(NEONATAL_death_outcome, paste0(output_folder,"NEONATAL_death.csv"))

}

