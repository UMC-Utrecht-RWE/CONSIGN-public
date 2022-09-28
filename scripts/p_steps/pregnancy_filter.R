#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/4/2022

# filters pregnancy table for 2018-present
# IDentifies historical and pandemic pregnancies
#identifies those non_pregnant during the pandemic for matching (cov_pos_non_preg)


# dates to determine cohorts (pandemic and before the pandemic)
start_date<-as.Date(as.character("20180101"), format = "%Y%m%d")
historical_end_date<-as.Date(as.character("20200101"), format = "%Y%m%d")
covid_start_date<-as.Date(as.character("20200301"), format = "%Y%m%d")

'%exclude%' <- function(x,y)!('%in%'(x,y))


CDM_source<-fread(paste0(path_CDM,"CDM_source.csv"))
DAP<-CDM_source$data_access_provider_name


my_PREG<-fread(paste0(path_CDM, "preg_trim.csv"))

OG_PREG_ID<-my_PREG$person_id

FC_OG_preg<-nrow(my_PREG)

FC_OG_mom<-length(unique(OG_PREG_ID))

# need persons to check for age from 12-55 at start pandemic

df_PERSONS<-IMPORT_PATTERN("PERSON", preselect_folder) 

OG_person_ID<-df_PERSONS$person_id 
FC_OG_person_ID<-length(OG_person_ID)

# pull year of birth from PERSONS, add to PREG, and use for exclusion

# add DOB criteria here
never_PREG_ID<-df_PERSONS$person_id[df_PERSONS$person_id%exclude%OG_PREG_ID==T]
FC_never_preg<-length(never_PREG_ID)

# NOT pregnant in 2020 from march-december (preg_end_date< march 1 2020) --> non_preg cohort
non_pan_preg_ID<- my_PREG$person_id[my_PREG$pregnancy_end_date<covid_start_date]
length(non_pan_preg_ID)

all_non_pan_preg_ID<-unique(c(never_PREG_ID, non_pan_preg_ID))
FC_all_non_pan_preg<-length(all_non_pan_preg_ID)

# Output of Create_Spells to measure follow up (FU) from preg_start_date
df_observation<-IMPORT_PATTERN("ALL_OBS", preselect_folder)


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

#############################################################################
# filter pregnancies without 1 year follow up from LMP (pregnancy_start_date)
# ###########################################################################

id_freq_preg<-as.numeric(table(my_PREG$person_id))

mother_obs<-df_observation[df_observation$person_id%in%my_PREG$person_id]
length(mother_obs$op_end_date)==length(id_freq_preg)

mother_op_end<-rep(mother_obs$op_end_date, id_freq_preg)

my_PREG$mother_op_end<-as.numeric(as.Date(mother_op_end,format = "%Y-%m-%d"))

my_PREG$preg_FU<-(my_PREG$mother_op_end)-(my_PREG$pregnancy_start_date)


my_PREG<- my_PREG[my_PREG$preg_FU>=365,]

FU_hist<-(hist(my_PREG$preg_FU, breaks=100,xlab="days", main="Follow Up Days from Start of Pregnancy"))
min(my_PREG$preg_FU)>=365

FC_sufficient_follow_up<-nrow(my_PREG)


# remove pregnancies that start before study period should this be based on start of pregnancy

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

# all the pregnancies included in the study with additional variables:

fwrite(my_PREG, paste0(path_CDM,"study_pop_PREG.csv"))


# filter CDM
# HELP check with Eimir (27/9)
# pandemic cohorts: pan_preg and non_preg they need to be 12-55 DURING pandemic: 55<2020- DOB>12
# historical cohort: 55< 2018-DOB>12

preg_ID<-unique(my_PREG$person_id)
pan_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="pandemic"])
hist_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="historical"])
between_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="between"])

actual_tables_preselect<-list()
actual_tables_preselect$EVENTS<-list.files(paste0(preselect_folder,"/"), pattern="^EVENTS_SLIM")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICAL_OBSERVATIONS_SLIM")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_OBSERVATIONS_SLIM")
actual_tables_preselect$MEDICINES<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICINES_SLIM")
actual_tables_preselect$VACCINES<-list.files(paste0(preselect_folder,"/"), pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(paste0(preselect_folder,"/"), pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(paste0(preselect_folder,"/"), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(preselect_folder,"/"), pattern = "\\.csv$")
table_list<-unlist(actual_tables_preselect)
##############################################################

# need to be between 12-55 at start of pandemic

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%pan_preg_ID,]
  fwrite(my_preg_table,paste0(pan_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$cohort=="pandemic",],paste0(pan_preg_folder,"my_PREG.csv"))

# need to be 12-55 at start of STUDY
for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%hist_preg_ID,]
  fwrite(my_preg_table,paste0(hist_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$cohort=="historical",],paste0(hist_preg_folder,"my_PREG.csv"))

  
# need to be 12-55 at start of PANDEMIC (only use this group for cov+ comparison)
for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%all_non_pan_preg_ID,]
  fwrite(my_preg_table,paste0(not_preg_folder,table_list[i]))
}


flowchart<-as.data.frame(cbind(FC_OG_person_ID, FC_OG_preg, FC_OG_mom, FC_preg_with_spell, 
                               FC_sufficient_follow_up, FC_from_start_study, FC_no_red_preg, length(pan_preg_ID), 
                               length(hist_preg_ID), length(between_preg_ID), FC_never_preg, FC_all_non_pan_preg))
                         
 colnames(flowchart)<-c("All women of reproductive age", "total pregnancies", "total mothers", "pregnancies with spell data",
                        "pregnancies 12 months follow up from LMP", "pregnancies starting during study period", "after excluding red pregnancies", 
                        "pandemic pregnancies","historical pregnancies", "between pregnancies", "women with no pregnancies EVER", "women with no pregnancy DURING pandemic") 
 
 fwrite(flowchart, paste0(output_dir,"flowchart_study_pop.csv"))
 