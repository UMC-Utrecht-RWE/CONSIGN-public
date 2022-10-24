#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#23/10/2022

# due to wide variety of DAP specific criteria, NEONATAL covariates will be coded separately per DAP

neonate_cohort_folders<-list(case_neonate_folder,control_neonate_folder, historical_neonate_folder)
output_folders<-neonate_cohort_folders

# my_neonate_data<-c("cov_pos_preg.csv", "cov_neg_preg.csv", "my_PREG.csv")

all_codes<-fread(paste0(projectFolder,"/ALL_full_codelist.csv"))
my_codes<-all_codes$code
no_dots_codes <- my_codes %>% str_replace_all('\\.', '')
all_codes$code_no_dots<-no_dots_codes

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
  
  
  #################################################################################
  # SGA or FGR
  
  SGA_FGR_names<-c("P_FGR_AESI")
  my_rows<-which(Reduce(`|`, lapply(SGA_FGR_names, startsWith, x = as.character(all_codes$full_name))))
  
  SGA_FGR_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))
  
  my_rows<-which(Reduce(`|`, lapply(SGA_FGR_codes, startsWith, x = as.character(EVENTS$event_code))))
  SGA_FGR_EV_ID<-(EVENTS$person_id[my_rows])
  SGA_FGR_EV_Date<- (EVENTS$start_date_record[my_rows])
 
  SGA_FGR_cov<-as.data.frame(cbind(SGA_FGR_EV_ID, SGA_FGR_EV_Date))
  colnames(SGA_FGR_cov)<-c("id", "date")
  fwrite(SGA_FGR_cov, paste0(output_folder,"SGA_FGR.csv"))
  
  #################################################################################
  # LBW
  # events and survey observations
  # so_source_table= "MDR" AND so_source_column == "peso"  AND so_source_value= <2500
  
  my_rows<-which(Reduce(`|`, lapply("P_LBW_AESI", startsWith, x = as.character(all_codes$full_name))))
  
  LBW_codes<-unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))
  
  my_rows<-which(Reduce(`|`, lapply(LBW_codes, startsWith, x = as.character(EVENTS$event_code))))
  LBW_EV_ID<-(EVENTS$person_id[my_rows])
  LBW_EV_Date<- (EVENTS$start_date_record[my_rows])
  

  
#####################################################################

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

