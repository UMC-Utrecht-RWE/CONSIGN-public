#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/4/2022

# filters pregnancy table for 2018-present
# identifies historical and pandemic pregnancies
# identifies trimester start and end dates

#filters CDM tables to include only person_ids with a pregnancy record
#IN PROGRESS

load(paste0(path_CDM,"D3_pregnancy_final.RData"))

study_start_date<- as.character("20180101")

start_date<-as.Date(study_start_date, format = "%Y%m%d")

covid_start_date<- as.character("20200101")

covid_date<-as.Date(covid_start_date, format = "%Y%m%d")


# help- eimir should this be based on start or end of pregnancy?
my_PREG<-D3_pregnancy_final[D3_pregnancy_final$pregnancy_start_date>start_date]

# establish pregnancy cohorts (historical or pandemic)
#help- eimir should this be based on start or end of pregnancy?

my_PREG$cohort<-NA

my_PREG$cohort[(my_PREG$pregnancy_end_date<covid_date)]<-"historical"

my_PREG$cohort[(my_PREG$pregnancy_end_date>=covid_date)]<-"pandemic"

# create trimesters

my_PREG$trim_1_start<- my_PREG$pregnancy_start_date
my_PREG$trim_1_end<- my_PREG$pregnancy_start_date+97
my_PREG$trim_1_end[my_PREG$trim_1_end>=my_PREG$pregnancy_end_date]<-NA


my_PREG$trim_2_start<- my_PREG$trim_1_end+1
my_PREG$trim_2_end<- my_PREG$trim_1_end+97
my_PREG$trim_2_end[my_PREG$trim_2_end>=my_PREG$pregnancy_end_date]<-NA

my_PREG$trim_3_start<- my_PREG$trim_2_end+1
my_PREG$trim_3_end<- my_PREG$pregnancy_end_date
my_PREG$trim_3_end[my_PREG$trim_2_end<my_PREG$trim_2_end]<-NA

# filter CDM

preg_id<-unique(my_PREG$person_id)


actual_tables_preselect<-list()
actual_tables_preselect$EVENTS<-list.files(paste0(preselect_folder,"/"), pattern="^EVENTS")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICAL_OBSERVATIONS")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_OBSERVATIONS")
actual_tables_preselect$MEDICINES<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICINES")
actual_tables_preselect$VACCINES<-list.files(paste0(preselect_folder,"/"), pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(paste0(preselect_folder,"/"), pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(paste0(preselect_folder,"/"), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(preselect_folder,"/"), pattern = "\\.csv$")
table_list<-unlist(actual_tables_preselect)



for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%preg_id,]
  fwrite(my_preg_table,paste0(preg_folder,table_list[i]))
}

'%exclude%' <- function(x,y)!('%in%'(x,y))

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_table_F<-my_table
  my_preg_table<- my_table[my_table$person_id%exclude%preg_id,]
  fwrite(my_preg_table,paste0(not_preg_folder,table_list[i]))
}