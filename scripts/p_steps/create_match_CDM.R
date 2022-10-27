#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#23/10/2022

# make CDM subsets for matched cohorts
#need all data during study period- source 


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

cov_control<-fread(paste0(matched_folder,"matches_cov_pos_non_preg.csv"))
preg_control<-fread(paste0(matched_folder,"matches_pregnant_cov_neg.csv"))
cases<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))

cov_control_id<-cov_control$person_id
preg_control_id<-preg_control$person_id
cases_id<-cases$person_id

preg_trim<-fread(paste0(projectFolder,"/preg_trim.csv"))
fwrite(preg_trim, paste0(preselect_folder, "preg_trim.csv"))
######################################################################################
if(DAP=="Bordeaux"){
actual_tables_CDM<-list()
actual_tables_CDM$EVENTS<-list.files(paste0(preselect_folder), pattern="^EVENTS")
actual_tables_CDM$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder), pattern="^MEDICAL_OB")
actual_tables_CDM$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder), pattern="^SURVEY_OB")
actual_tables_CDM$MEDICINES<-list.files(paste0(preselect_folder), pattern="^MEDICINES")
actual_tables_CDM$VACCINES<-list.files(paste0(preselect_folder), pattern="^VACCINES")
actual_tables_CDM$SURVEY_ID<-list.files(paste0(preselect_folder), pattern="^SURVEY_ID")
actual_tables_CDM$EUROCAT<-list.files(paste0(preselect_folder), pattern="^EUROCAT")
actual_tables_CDM$PERSONS<-list.files(paste0(preselect_folder), pattern="^PERSONS.csv")
actual_tables_CDM$PREGNANCY<-list.files(paste0(preselect_folder), pattern="^preg_trim")}else{
  actual_tables_CDM<-list()
  actual_tables_CDM$EVENTS<-list.files(paste0(preselect_folder), pattern="^EVENTS_SLIM")
  actual_tables_CDM$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder), pattern="^MED_OB_SLIM")
  actual_tables_CDM$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder), pattern="^SURVEY_SLIM")
  actual_tables_CDM$MEDICINES<-list.files(paste0(preselect_folder), pattern="^MEDICINES_SLIM")
  actual_tables_CDM$VACCINES<-list.files(paste0(preselect_folder), pattern="^VACCINES")
  actual_tables_CDM$SURVEY_ID<-list.files(paste0(preselect_folder), pattern="^SURVEY_ID")
  actual_tables_CDM$EUROCAT<-list.files(paste0(preselect_folder), pattern="^EUROCAT")
  actual_tables_CDM$PERSONS<-list.files(paste0(preselect_folder), pattern="^PERSONS.csv")
  actual_tables_CDM$PREGNANCY<-list.files(paste0(preselect_folder), pattern="^preg_trim")
}


table_list<-unlist(actual_tables_CDM)

#########################################################################################
for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_match_table<- my_table[(my_table$person_id%in%cov_control_id),]
  
  fwrite(my_match_table,paste0(cov_match_folder,table_list[i]))
}

########################################################################################

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_match_table<- my_table[(my_table$person_id%in%preg_control_id),]
  
  fwrite(my_match_table,paste0(preg_match_folder,table_list[i]))
}

########################################################################################

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_match_table<- my_table[(my_table$person_id%in%cases_id),]
  
  fwrite(my_match_table,paste0(cases_match_folder,table_list[i]))
}

