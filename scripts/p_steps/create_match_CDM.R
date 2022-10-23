#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#23/10/2022

# make CDM subsets for matched cohorts

# for cases this in pan_preg_cov_pos

cov_control<-fread(paste0(matched_folder,"matches_cov_pos_non_preg.csv"))
preg_control<-fread(paste0(matched_folder,"matches_pregnant_cov_neg.csv"))

cov_control_id<-cov_control$person_id
preg_control_id<-cov_control$person_id


cov_match_folder<-paste0(matched_folder,"CDM_covid_positive/")
invisible(if(dir.exists(cov_match_folder)==F)
{dir.create(cov_match_folder)})

preg_match_folder<-paste0(matched_folder,"CDM_preg_negative/")
invisible(if(dir.exists(preg_match_folder)==F)
{dir.create(preg_match_folder)})

######################################################################################

actual_tables_CDM<-list()
actual_tables_CDM$EVENTS<-list.files(paste0(cov_pos_not_preg_folder), pattern="^EVENTS")
actual_tables_CDM$MEDICAL_OBSERVATIONS<-list.files(paste0(cov_pos_not_preg_folder), pattern="^MEDICAL_OB")
actual_tables_CDM$SURVEY_OBSERVATIONS<-list.files(paste0(cov_pos_not_preg_folder), pattern="^SURVEY_OB")
actual_tables_CDM$MEDICINES<-list.files(paste0(cov_pos_not_preg_folder), pattern="^MEDICINES")
actual_tables_CDM$VACCINES<-list.files(paste0(cov_pos_not_preg_folder), pattern="^VACCINES")
actual_tables_CDM$SURVEY_ID<-list.files(paste0(cov_pos_not_preg_folder), pattern="^SURVEY_ID")
actual_tables_CDM$EUROCAT<-list.files(paste0(cov_pos_not_preg_folder), pattern="^EUROCAT")
actual_tables_CDM$PERSONS<-list.files(paste0(cov_pos_not_preg_folder), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(cov_pos_not_preg_folder), pattern = "\\.csv$")
table_list<-unlist(actual_tables_CDM)

for (i in 1:length(table_list)){
  my_table<-fread(paste0(cov_pos_not_preg_folder,table_list[i]))
  my_match_table<- my_table[(my_table$person_id%in%cov_control_id),]
  
  fwrite(my_match_table,paste0(cov_match_folder,table_list[i]))
}

########################################################################################

actual_tables_CDM<-list()
actual_tables_CDM$EVENTS<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^EVENTS")
actual_tables_CDM$MEDICAL_OBSERVATIONS<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^MEDICAL_OB")
actual_tables_CDM$SURVEY_OBSERVATIONS<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^SURVEY_OB")
actual_tables_CDM$MEDICINES<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^MEDICINES")
actual_tables_CDM$VACCINES<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^VACCINES")
actual_tables_CDM$SURVEY_ID<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^SURVEY_ID")
actual_tables_CDM$EUROCAT<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^EUROCAT")
actual_tables_CDM$PERSONS<-list.files(paste0(cov_neg_pan_preg_folder), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(cov_neg_pan_preg_folder), pattern = "\\.csv$")
table_list<-unlist(actual_tables_CDM)

for (i in 1:length(table_list)){
  my_table<-fread(paste0(cov_neg_pan_preg_folder,table_list[i]))
  my_match_table<- my_table[(my_table$person_id%in%cov_control_id),]
  
  fwrite(my_match_table,paste0(preg_match_folder,table_list[i]))
}


