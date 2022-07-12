#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/4/2022

# filters pregnancy table for 2018-present
# IDentifies historical and pandemic pregnancies
# IDentifies trimester start and end dates

#filters CDM tables to include only person_IDs with a pregnancy record
#IN PROGRESS

load(paste0(path_CDM,"D3_pregnancy_final.RData"))


start_date<-as.Date(as.character("20180101"), format = "%Y%m%d")
historical_end_date<-as.Date(as.character("20200101"), format = "%Y%m%d")

covid_date<-as.Date(as.character("20200301"), format = "%Y%m%d")


# should this be based on start or end of pregnancy?
#answer: start
my_PREG<-D3_pregnancy_final[D3_pregnancy_final$pregnancy_start_date>start_date]

#filter out non-green quality pregnancies
#HELP: should this be only green or green and yellow?

my_PREG<-my_PREG[my_PREG$pregnancy_id%like%"Green",]

# establish pregnancy cohorts (historical or pandemic)
#help- eimir should this be based on start or end of pregnancy?

my_PREG$cohort<-NA

my_PREG$cohort[(my_PREG$pregnancy_end_date<historical_end_date)]<-"historical"

my_PREG$cohort[(my_PREG$pregnancy_end_date>=covid_date)]<-"pandemic"

my_PREG$cohort[is.na(my_PREG$cohort)]<-"between"

# create trimesters

my_PREG$trim_1_start<- my_PREG$pregnancy_start_date
my_PREG$trim_1_end<- my_PREG$pregnancy_start_date+97
my_PREG$trim_1_end[my_PREG$trim_1_end>=my_PREG$pregnancy_end_date]<-NA


my_PREG$trim_2_start<- my_PREG$trim_1_end+1
my_PREG$trim_2_end<- my_PREG$trim_1_end+97
my_PREG$trim_2_end[my_PREG$trim_2_end>=my_PREG$pregnancy_end_date]<-NA

my_PREG$trim_3_start<- my_PREG$trim_2_end+1
my_PREG$trim_3_end<- my_PREG$pregnancy_end_date
my_PREG$trim_3_end[is.na(my_PREG$trim_3_start)]<- NA
my_PREG$trim_3_end[my_PREG$trim_3_end<my_PREG$trim_2_end]<-NA

# filter CDM
preg_ID<-unique(my_PREG$person_id)
pan_preg_ID<-unique(my_PREG$person_ID[my_PREG$cohort=="pandemic"])
hist_preg_ID<-unique(my_PREG$person_ID[my_PREG$cohort=="historical"])

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

fwrite(my_PREG, paste0(path_CDM,"all_PREG.csv"))

all_person_ID<-list()
for(i in 1:length(actual_tables_preselect$PERSONS)){
  my_table<-fread(paste0(preselect_folder,actual_tables_preselect$PERSONS[1]))
  all_person_ID[[i]]<-unique(my_table$person_ID)
}

all_person_ID<-unlist(all_person_ID)


for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_ID%in%pan_preg_ID,]
  fwrite(my_preg_table,paste0(pan_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$cohort=="pandemic",],paste0(pan_preg_folder,"my_PREG.csv"))

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_ID%in%hist_preg_ID,]
  fwrite(my_preg_table,paste0(hist_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$cohort=="historical",],paste0(hist_preg_folder,"my_PREG.csv"))

'%exclude%' <- function(x,y)!('%in%'(x,y))

non_preg_ID<-all_person_ID[all_person_ID%exclude%preg_ID]
non_preg_hist_ID<-hist_preg_ID[hist_preg_ID%exclude%pan_preg_ID]

all_non_preg_ID<- unique(c(non_preg_ID, non_preg_hist_ID))

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_table_F<-my_table
  my_preg_table<- my_table[my_table$person_ID%in%all_non_preg_ID,]
  fwrite(my_preg_table,paste0(not_preg_folder,table_list[i]))
}

