#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/4/2022

# filters pregnancy table for 2018-present
# IDentifies historical and pandemic pregnancies


#filters CDM tables to include only person_IDs with a pregnancy record
#IN PROGRESS

CDM_source<-fread(paste0(path_CDM,"CDM_source.csv"))
DAP<-CDM_source$data_access_provider_name


my_PREG<-fread(paste0(path_CDM, "preg_trim.csv"))
FC_OG_preg_id<-nrow(my_PREG)

# Output of Create_Spells to measure follow up (FU) from preg_start_date
df_observation<-IMPORT_PATTERN("ALL_OBS", preselect_folder)
# check that preselect filters and copies ALL_OBS

# need persons to check for age from 12-55 at start pandemic
#also need to check for cov_pos_non_preg
df_PERSONS<-IMPORT_PATTERN("PERSON", preselect_folder)

start_date<-as.Date(as.character("20180101"), format = "%Y%m%d")
historical_end_date<-as.Date(as.character("20200101"), format = "%Y%m%d")

covid_start_date<-as.Date(as.character("20200301"), format = "%Y%m%d")

#######################
# make categorical maternal_age (at start of pregnancy) groups
# HELP this grouping is arbitrary, age-group definition missing from SAP, please advise DONE 12/9
# page 30 "covariates"
# 12-24 years of age
# 25-39 years of age
# 40-55 years of age

my_PREG$age_group<-my_PREG$age_at_start_of_pregnancy

my_PREG$age_group[between(my_PREG$age_at_start_of_pregnancy, 12,24)]<-1

my_PREG$age_group[between(my_PREG$age_at_start_of_pregnancy, lower=25, upper=39)]<-2

my_PREG$age_group[between(my_PREG$age_at_start_of_pregnancy,40,55)]<-3

#############################################

# person_ids must be in observations
my_PREG<-my_PREG[my_PREG$person_id%in%df_observation$person_id]

FC_preg_with_spell<-nrow(my_PREG)

# filter pregnancies without 1 year follow up from LMP (pregnancy_start_date)
# 

id_freq_preg<-as.numeric(table(my_PREG$person_id))

mother_obs<-df_observation[df_observation$person_id%in%my_PREG$person_id]
length(mother_obs$op_end_date)==length(id_freq_preg)

mother_op_end<-rep(mother_obs$op_end_date, id_freq_preg)

my_PREG$mother_op_end<-as.numeric(as.Date(mother_op_end,format = "%Y-%m-%d"))

my_PREG$preg_FU<-(my_PREG$mother_op_end)-(my_PREG$pregnancy_start_date)
FU_hist<-(hist(my_PREG$preg_FU, breaks=50,xlab="days", main="Follow Up Days from Start of Pregnancy"))


my_PREG<- my_PREG[my_PREG$preg_FU>=365,]

FC_sufficient_follow_up<-nrow(my_PREG)


# remove pregnancies that start before study period should this be based on start or end of pregnancy?
#answer: start
my_PREG<-my_PREG[my_PREG$pregnancy_start_date>=start_date]

FC_from_start_study<- nrow(my_PREG)


#filter out red quality pregnancies (DAP specific due to data generating mechanism which makes "red")

if(DAP!="ARS"){
my_PREG<-my_PREG[(my_PREG$pregnancy_id%like%"Red")==F,]
FC_no_red_preg<-nrow(my_PREG)}else{no_red_preg<- "ARS keeps red pregnancies"}

# establish pregnancy cohorts (historical or pandemic)
#help- eimir should this be based on start or end of pregnancy?

my_PREG$cohort<-NA

my_PREG$cohort[(my_PREG$pregnancy_end_date<historical_end_date)]<-"historical"

my_PREG$cohort[(my_PREG$pregnancy_end_date>=covid_start_date)]<-"pandemic"

my_PREG$cohort[is.na(my_PREG$cohort)]<-"between"



# filter CDM
preg_ID<-unique(my_PREG$person_id)
pan_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="pandemic"])
hist_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="historical"])
between_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="between"])

actual_tables_preselect<-list()
actual_tables_preselect$EVENTS<-IMPORT_PATTERN("EVENTS")
  list.files(paste0(preselect_folder,"/"), pattern="^EVENTS")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICAL_OBSERVATIONS")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_OBSERVATIONS")
actual_tables_preselect$MEDICINES<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICINES")
actual_tables_preselect$VACCINES<-list.files(paste0(preselect_folder,"/"), pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(paste0(preselect_folder,"/"), pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(paste0(preselect_folder,"/"), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(preselect_folder,"/"), pattern = "\\.csv$")
table_list<-unlist(actual_tables_preselect)

fwrite(my_PREG, paste0(path_CDM,"study_pop_PREG.csv"))

OG_person_ID<-list()
for(i in 1:length(actual_tables_preselect$PERSONS)){
  my_table<-fread(paste0(path_CDM,actual_tables_preselect$PERSONS[i]))
  OG_person_ID[[i]]<-unique(my_table$person_id)
}

OG_person_ID%in%preg_ID
FC_OG_person_ID<-length(unique(unlist(OG_person_ID)))


preselect_person_ID<-list()
for(i in 1:length(actual_tables_preselect$PERSONS)){
  my_table<-fread(paste0(preselect_folder,actual_tables_preselect$PERSONS[i]))
  preselect_person_ID[[i]]<-unique(my_table$person_id)
}

FC_preselect_person_ID<-(unique(unlist(preselect_person_ID)))


for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%pan_preg_ID,]
  fwrite(my_preg_table,paste0(pan_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$cohort=="pandemic",],paste0(pan_preg_folder,"my_PREG.csv"))

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%hist_preg_ID,]
  fwrite(my_preg_table,paste0(hist_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$cohort=="historical",],paste0(hist_preg_folder,"my_PREG.csv"))

'%exclude%' <- function(x,y)!('%in%'(x,y))

non_preg_ID<-unique(unlist(preselect_person_ID[preselect_person_ID%exclude%preg_ID]))
# what's this step? #HELP #check 
non_pan_preg_ID<-unique(unlist(preselect_person_ID[preselect_person_ID%exclude%(pan_preg_ID)]))


all_non_preg_ID<- unlist(c(non_preg_ID, non_preg_hist_ID))
FC_all_non_preg_ID<- length(unique(non_preg_ID))
FC_pan_non_preg_ID<-length(unique(non_pan_preg_ID))

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%non_pan_preg_ID,]
  fwrite(my_preg_table,paste0(not_preg_folder,table_list[i]))
}


flowchart<-as.data.frame(cbind(FC_OG_person_ID, length(FC_preselect_person_ID), FC_OG_preg_id, FC_preg_with_spell, FC_sufficient_follow_up, FC_from_start_study, FC_no_red_preg, length(pan_preg_ID), 
                               length(hist_preg_ID), length(between_preg_ID), FC_all_non_preg_ID, FC_pan_non_preg_ID))
                         
 colnames(flowchart)<-c("Original PERSONS", "preselect (women of reproductive age)", "total pregnancies", "pregnancies with spell data",
                        "pregnancies 12 months follow up from LMP", "pregnancies during study period", "after excluding red pregnancies", 
                        "pandemic pregnancies","historical pregnancies", "between pregnancies", "women with no pregnancies EVER", "women with no pregnancy DURING pandemic") 
 
 fwrite(flowchart, paste0(output_dir,"flowchart_study_pop.csv"))
 