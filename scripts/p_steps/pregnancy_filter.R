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


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name


my_PREG<-fread(paste0(path_CDM, "preg_trim.csv"))

OG_PREG_ID<-my_PREG$person_id

FC_OG_preg<-nrow(my_PREG)

FC_OG_mom<-length(unique(OG_PREG_ID))

# need persons to check for age from 12-55 at start pandemic

df_PERSONS<-IMPORT_PATTERN("PERSON", preselect_folder) 

df_PERSONS$DOB<-as.Date(paste0(df_PERSONS$day_of_birth,df_PERSONS$month_of_birth, df_PERSONS$year_of_birth), format = "%d%m%Y")

OG_person_ID<-df_PERSONS$person_id 
FC_OG_person_ID<-length(OG_person_ID)

# pull year of birth from PERSONS, add to PREG, and use for exclusion

# add DOB criteria here
never_PREG_ID<-df_PERSONS$person_id[df_PERSONS$person_id%exclude%OG_PREG_ID==T]
FC_never_preg<-length(never_PREG_ID)

# NOT pregnant in 2020 from march-december (preg_end_date< jan 1 2020) --> non_preg cohort
non_pan_preg_ID<- my_PREG$person_id[my_PREG$pregnancy_end_date<historical_end_date]
length(non_pan_preg_ID)

all_non_pan_preg_ID<-unique(c(never_PREG_ID, non_pan_preg_ID))
FC_all_non_pan_preg<-length(all_non_pan_preg_ID)

# Output of Create_Spells to measure follow up (FU) from preg_start_date
df_observation<-IMPORT_PATTERN("ALL_OBS", preselect_folder)


#######################
# make categorical maternal_age (at start of pregnancy) groups: page 30 "covariates"
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

if (min(my_PREG$preg_FU)>=365){print("Follow Up from LMP at least 365 days, OK")}else{print("Follow Up Restriction Failure")}

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

fwrite(my_PREG, paste0(projectFolder,"/study_pop_PREG.csv"))


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
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^MED_OB_SLIM")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_SLIM")
actual_tables_preselect$MEDICINES<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICINES_SLIM")
actual_tables_preselect$VACCINES<-list.files(paste0(preselect_folder,"/"), pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(paste0(preselect_folder,"/"), pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(paste0(preselect_folder,"/"), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(preselect_folder,"/"), pattern = "\\.csv$")
table_list<-unlist(actual_tables_preselect)
##############################################################

# need to be between 12-55 at start of pandemic

# bring in personsfilter() function, make a filter_id for each cohort and filter cohort_ids

personsfilter<-function(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=(2018-55), dobmax=(2020-12)) {
  newdata<-personstable[(personstable[,get(sex)]==female),]
  flowchart_gender<-as.numeric(c(nrow(personstable), nrow(newdata)))
  newdata<-newdata[(newdata[,get(dob)]>=dobmin),]
  filtered_data<-newdata[(newdata[,get(dob)]<=dobmax),]
  flowchart_age<-as.numeric(c(flowchart_gender, nrow(filtered_data)))
  flowchart_steps<-c("original", "females only", "1954<=DOB<=2008")
  filter_ID<-(filtered_data[[caseid]])
  flowchart_filter<-as.data.frame(cbind(flowchart_steps, flowchart_age))
  colnames(flowchart_filter)<-c("filter_step","cases_number" )
  persons_filter_output<-list(filter_ID, flowchart_filter, filtered_data)
  return(persons_filter_output)
}

df_PERSONS_preg<- df_PERSONS[df_PERSONS$person_id%in%my_PREG$person_id,]

# PANDEMIC PREGNANT
age_filter_pandemic<-personsfilter(personstable = df_PERSONS_preg,caseid = "person_id", female="F", dob= "year_of_birth", dobmin = (2020-55), dobmax = (2020-12))
age_pan_ID<-unlist(age_filter_pandemic[1])
pan_preg_ID_age<-pan_preg_ID[pan_preg_ID%in%age_pan_ID]

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%pan_preg_ID_age,]
  
  fwrite(my_preg_table,paste0(pan_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$person_id%in%pan_preg_ID_age,],paste0(pan_preg_folder,"my_PREG.csv"))

# NON-PREGNANT 
# need to be 12-55 at start of PANDEMIC (only use this group for cov+ comparison)

non_pan_preg_ID_age<-all_non_pan_preg_ID[all_non_pan_preg_ID%in%age_pan_ID]
for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%non_pan_preg_ID_age,]
  fwrite(my_preg_table,paste0(not_preg_folder,table_list[i]))
}

# HISTORICAL PREGNANT
# historical group age filter for observation time 2018-2020
age_filter_hist<-personsfilter(personstable = df_PERSONS_preg,caseid = "person_id", female="F", dob= "year_of_birth", dobmin = (2018-55), dobmax = (2018-12))
age_hist_ID<-unlist(age_filter_hist[1])
hist_preg_ID_age<-hist_preg_ID[hist_preg_ID%in%age_hist_ID]


# need to be 12-55 at start of STUDY 2018
for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%hist_preg_ID_age,]
  fwrite(my_preg_table,paste0(hist_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$person_id%in%hist_preg_ID_age,],paste0(hist_preg_folder,"my_PREG.csv"))


# #######################################################
# FLOWCHART to record attrition
#########################################################

flowchart<-as.data.frame(cbind(FC_OG_person_ID, FC_OG_preg, FC_OG_mom, FC_preg_with_spell, 
                               FC_sufficient_follow_up, FC_from_start_study, FC_no_red_preg, length(pan_preg_ID), length(pan_preg_ID_age),
                               length(hist_preg_ID),length(hist_preg_ID_age), length(between_preg_ID), FC_never_preg, FC_all_non_pan_preg, length(non_pan_preg_ID_age)))
                         
 colnames(flowchart)<-c("All women of reproductive age", "total pregnancies", "total mothers", "pregnancies with spell data",
                        "pregnancies 12 months follow up from LMP", "pregnancies starting during study period", "after excluding red pregnancies", 
                        "pandemic pregnancies","pandemic pregnancies with age 12-55 in 2020","historical pregnancies",
                        "historical pregnancies with age 12-55 in 2018","between pregnancies", "women with no pregnancies EVER", "women with no pregnancy DURING pandemic", "women with no pregnancy DURING pandemic age 12-55 in 2020") 
 
 fwrite(flowchart, paste0(output_dir,"flowchart_study_pop.csv"))
 
 # before removing the preselect folder, copy cov_data out to projectFolder
 
 # cov_data<-fread(paste0(preselect_folder,"covid_data.csv"))
 
 # fwrite(cov_data, paste0(projectFolder, "/covid_data.csv"))
 
 #########################################################
 
 # delete preselect
 
 unlink( paste0(projectFolder,"/CDMInstances_preselect"), recursive=T)
 
 