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

# Output of Create_Spells to measure follow up (FU) from preg_start_date
df_observation<-IMPORT_PATTERN("ALL_OBS", preselect_folder)


start_date<-as.Date(as.character("20180101"), format = "%Y%m%d")
historical_end_date<-as.Date(as.character("20200101"), format = "%Y%m%d")

covid_date<-as.Date(as.character("20200301"), format = "%Y%m%d")

#######################
# make categorical maternal_age (at start of pregnancy) groups
# HELP this grouping is arbitrary, age-group definition missing from SAP, please advise

my_PREG$age_group<-my_PREG$age_at_start_of_pregnancy

my_PREG$age_group[my_PREG$age_at_start_of_pregnancy<20]<-1

my_PREG$age_group[between(my_PREG$age_at_start_of_pregnancy, lower=20, upper=35)]<-2

my_PREG$age_group[my_PREG$age_at_start_of_pregnancy>35]<-3

# table(my_PREG$age_group)
############################################
OG_preg_id<-length(unique(my_PREG$person_id))
#############################################

# person_ids must be in observations
my_PREG<-my_PREG[my_PREG$person_id%in%df_observation$person_id]



# filter pregnancies without 1 year follow up from LMP (pregnancy_start_date)
# 
# preg_wide<-dcast(setDT(my_PREG), person_id ~ rowid(person_id), value.var = ("pregnancy_start_date"))
# preg_names<-vector()
# for(i in 2:ncol(preg_wide)){
#   preg_names[i-1]<-paste0("FU_pregnancy_",(i-1))
# }
# 
# colnames(preg_wide)<-c("person_id",preg_names)
# preg_wide[order(person_id),] 
# 
# df_observation<-df_observation[df_observation$person_id%in%preg_wide$person_id]
# df_observation[order(person_id),]
# 
# if((all(df_observation$person_id==preg_wide$person_id))==F){print("person_id match failure");break}else{print("id match OK")}
# 
# num_end_date<-as.matrix(as.numeric(as.Date(df_observation$op_end_date, format="%Y-%m-%d")))
# 
# FU_from_LMP<-as.data.frame(unlist(apply(num_end_date,2,function(x) preg_wide[,2:ncol(preg_wide)] - x )))
# colnames(FU_from_LMP)<-preg_names
# df_FU<-as.data.frame(cbind(preg_wide$person_id, FU_from_LMP))
# colnames(df_FU)<-c("person_id", preg_names)
# 
# 
# df_FU_long<-melt.data.frame(df_FU, id.vars = c("person_id"),measure.vars = preg_names, na.rm = T)
# df_FU_long[order("person_id"),]
# 
# if((all(df_FU_long$person_id==preg_wide$person_id))==F){print("person_id match failure");break}else{print("id match OK")}
# 
# my_PREG$days_FU_from_LMP<-df_FU_long$value
# 
# preg_id_FU<-length(unique(my_PREG$person_id))

# remove pregnancies that start before study period should this be based on start or end of pregnancy?
#answer: start
my_PREG<-my_PREG[my_PREG$pregnancy_start_date>=start_date]

study_PREG_ID<- length(unique(my_PREG$person_id))


#filter out red quality pregnancies (DAP specific due to data generating mechanism which makes "red")

if(DAP!="ARS"){
my_PREG<-my_PREG[(my_PREG$pregnancy_id%like%"Red")==F,]
no_red_preg<-length(unique(my_PREG$person_id))}else{no_red_preg<- "ARS keeps red pregnancies"}

# establish pregnancy cohorts (historical or pandemic)
#help- eimir should this be based on start or end of pregnancy?

my_PREG$cohort<-NA

my_PREG$cohort[(my_PREG$pregnancy_end_date<historical_end_date)]<-"historical"

my_PREG$cohort[(my_PREG$pregnancy_end_date>=covid_date)]<-"pandemic"

my_PREG$cohort[is.na(my_PREG$cohort)]<-"between"



# filter CDM
preg_ID<-unique(my_PREG$person_id)
pan_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="pandemic"])
hist_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="historical"])
between_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="between"])

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

fwrite(my_PREG, paste0(path_CDM,"study_pop_PREG.csv"))

OG_person_ID<-list()
for(i in 1:length(actual_tables_preselect$PERSONS)){
  my_table<-fread(paste0(path_CDM,actual_tables_preselect$PERSONS[i]))
  OG_person_ID[[i]]<-unique(my_table$person_id)
}

OG_person_ID%in%preg_ID
OG_person_ID<-length(unique(unlist(OG_person_ID)))


preselect_person_ID<-list()
for(i in 1:length(actual_tables_preselect$PERSONS)){
  my_table<-fread(paste0(preselect_folder,actual_tables_preselect$PERSONS[i]))
  preselect_person_ID[[i]]<-unique(my_table$person_id)
}

preselect_person_ID<-(unique(unlist(preselect_person_ID)))


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

non_preg_ID<-preselect_person_ID[preselect_person_ID%exclude%preg_ID]
# what's this step? #HELP #check 
non_preg_hist_ID<-hist_preg_ID[hist_preg_ID%exclude%pan_preg_ID]

all_non_preg_ID<- unique(c(non_preg_ID, non_preg_hist_ID))

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%all_non_preg_ID,]
  fwrite(my_preg_table,paste0(not_preg_folder,table_list[i]))
}


flowchart<-as.data.frame(cbind((OG_person_ID),length(preselect_person_ID), OG_preg_id,study_PREG_ID, no_red_preg, length(pan_preg_ID), 
                               length(hist_preg_ID), length(between_preg_ID), length(non_preg_ID)))
                         
 colnames(flowchart)<-c("Original PERSONS", "preselect (women of reproductive age)", "Women who had at least one pregnancy",
                        "women who had pregnancy during study period", "after excluding red pregnancies", "women with pandemic pregnancies",
                        "women with historical pregnancies", "women with between pregnancies", "women without pregnancy") 
 
 fwrite(flowchart, paste0(output_dir,"flowchart_study_pop.csv"))