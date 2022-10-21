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

############################################################
# IMPORT DATA

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

df_observation<-fread(paste0(preselect_folder,"ALL_OBS_SPELLS.csv"))
df_PERSONS<-IMPORT_PATTERN("PERSON", preselect_folder)
my_PREG<-fread(paste0(projectFolder, "/preg_trim.csv"))
############################################################

# store original (preselect) flowchart data  "All women of reproductive age between 2018-2021"  "total pregnancies" "total mothers"               
# PERSONS
OG_person_ID<-df_PERSONS$person_id 
FC_OG_person_ID<-length(OG_person_ID)

# PREGNANCY
OG_PREG_ID<-my_PREG$person_id
FC_OG_preg<-nrow(my_PREG)
FC_OG_mom<-length(unique(OG_PREG_ID))

###############################################
# exclude gestage>44 weeks "pregnancies excluded due to gestation>44 weeks"  

print("are there pregnancies longer than 44 weeks?")
print(table(my_PREG$gestage_greater_44))
FC_exclude_44<-nrow(my_PREG[my_PREG$gestage_greater_44==1,])
print("exclude any pregnancies with gestage_greater_44 == 1")
my_PREG<-my_PREG[my_PREG$gestage_greater_44==0,]

#############################################################################

# person_ids must be in observations
my_PREG<-my_PREG[my_PREG$person_id%in%df_observation$person_id]

FC_preg_with_spell<-nrow(my_PREG)

##############################################################################

#filter out red quality pregnancies (DAP specific due to data generating mechanism which makes "red")
FC_no_red_preg<-NA

if(DAP!="ARS"){
  my_PREG<-my_PREG[(my_PREG$pregnancy_id%like%"Red")==F,]
  FC_no_red_preg<-nrow(my_PREG)}else{no_red_preg<- "ARS keeps red pregnancies"}



# ###########################################################################
# "pregnancies starting at least 12 months before end of study"
# 10/10 need to measure maternal death (means end of follow up) can't exclude on follow up
# --> preg_start_date<31/12/2020

max_preg_start_date<-as.Date(as.character("20201231"), format = "%Y%m%d")

my_PREG<-my_PREG[my_PREG$pregnancy_start_date<=max_preg_start_date,]

FC_sufficient_follow_up<-nrow(my_PREG)

###########################################################################

# adding pregnancy 0/1 to PERSONS for subsetting

df_PERSONS$pregnant_ever<-0
df_PERSONS$pregnant_ever[df_PERSONS$person_id%in%my_PREG$person_id]<-1
table(df_PERSONS$pregnant_ever)


# add elligibility criteria from CreateEntryExit (in PERSONS) to my_PREG
my_PREG$elligible_hist<-0
ell_hist_preg_id<-my_PREG$person_id[df_PERSONS$elligible_historical==1]
my_PREG$elligible_hist[my_PREG$person_id%in%ell_hist_preg_id]<-1
table(my_PREG$elligible_hist)

my_PREG$elligible_pandemic<-0
ell_pan_preg_id<-my_PREG$person_id[df_PERSONS$elligible_pandemic==1]
my_PREG$elligible_pandemic[my_PREG$person_id%in%ell_pan_preg_id]<-1

# NEVER PREG ONLY IF ELLIGIBLE FOR PANDEMIC COHORT add entry/exit criteria here

never_PREG_ID<-df_PERSONS$person_id[df_PERSONS$pregnant_ever==0 & df_PERSONS$elligible_pandemic==1]
FC_never_preg<-length(never_PREG_ID)

# NOT pregnant in 2020 from march-december (preg_end_date< jan 1 2020) --> non_preg cohort
non_pan_preg_ID<- my_PREG$person_id[my_PREG$pregnancy_end_date<historical_end_date &my_PREG$elligible_pandemic==1]
length(unique(non_pan_preg_ID))

all_non_pan_preg_ID<-unique(c(never_PREG_ID, non_pan_preg_ID))
FC_all_non_pan_preg<-length(all_non_pan_preg_ID)



#######################
# make categorical maternal_age (at start of pregnancy) groups: SAP page 30 "covariates"
# 12-24 years of age
# 25-39 years of age
# 40-55 years of age

my_PREG$age_group<-my_PREG$age_at_start_of_pregnancy

my_PREG$age_group[between(my_PREG$age_at_start_of_pregnancy, 12,24)]<-1

my_PREG$age_group[between(my_PREG$age_at_start_of_pregnancy, lower=25, upper=39)]<-2

my_PREG$age_group[between(my_PREG$age_at_start_of_pregnancy,40,55)]<-3



# establish pregnancy cohorts (historical or pandemic)
#help- eimir should this be based on start or end of pregnancy?

my_PREG$cohort<-NA

my_PREG$cohort[(my_PREG$pregnancy_end_date<historical_end_date)]<-"historical"

my_PREG$cohort[(my_PREG$pregnancy_end_date>=covid_start_date)]<-"pandemic"

my_PREG$cohort[is.na(my_PREG$cohort)]<-"between"


# all the pregnancies included in the study with additional variables:

fwrite(my_PREG, paste0(g_intermediate,"/study_pop_PREG.csv"))


# filter CDM
# HELP check with Eimir (27/9)
# IMPROVE: use entry and exit dates instead of DOB
# pandemic cohorts: pan_preg and non_preg they need to be 12-55 DURING pandemic: 55<2020- DOB>12
# historical cohort: 55< 2018-DOB>12

preg_ID<-unique(my_PREG$person_id)
pan_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="pandemic"])
hist_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="historical"])
between_preg_ID<-unique(my_PREG$person_id[my_PREG$cohort=="between"])

df_PERSONS_preg<- df_PERSONS[df_PERSONS$person_id%in%my_PREG$person_id,]


actual_tables_preselect<-list()

# CHECK WHICH DAPS USE "LARGE" VERSION

if(DAP!="Bordeaux"){
actual_tables_preselect$EVENTS<-list.files(paste0(preselect_folder,"/"), pattern="^EVENTS_SLIM")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^MED_OB_SLIM")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_SLIM")
actual_tables_preselect$MEDICINES<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICINES_SLIM")}else{
  actual_tables_preselect$EVENTS<-list.files(paste0(preselect_folder,"/"), pattern="^EVENTS")
  actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICAL_OB")
  actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_OB")
  actual_tables_preselect$MEDICINES<-list.files(paste0(preselect_folder,"/"), pattern="^MEDICINES")
}


actual_tables_preselect$VACCINES<-list.files(paste0(preselect_folder,"/"), pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(paste0(preselect_folder,"/"), pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(paste0(preselect_folder,"/"), pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(paste0(preselect_folder,"/"), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(preselect_folder,"/"), pattern = "\\.csv$")
table_list<-unlist(actual_tables_preselect)
##############################################################

# need to be between 12-55 at start of pandemic - PERSONS$elligible_pandemic

# PANDEMIC PREGNANT

pan_preg_ID_age<-my_PREG$person_id[(my_PREG$cohort=="pandemic")& (my_PREG$elligible_pandemic==1)]


for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[(my_table$person_id%in%pan_preg_ID_age),]
  
  fwrite(my_preg_table,paste0(pan_preg_folder,table_list[i]))
}

pan_PREGNANCIES_age<-nrow(my_PREG[my_PREG$person_id%in%pan_preg_ID_age,])
fwrite(my_PREG[((my_PREG$person_id%in%pan_preg_ID_age)&(my_PREG$cohort=="pandemic")),],paste0(pan_preg_folder,"my_PREG.csv"))

# NON-PREGNANT 
# need to be 12-55 at start of PANDEMIC (only use this group for cov+ comparison)
age_pan_ID<-df_PERSONS$person_id[df_PERSONS$elligible_pandemic==1]
non_pan_preg_ID_age<-all_non_pan_preg_ID[all_non_pan_preg_ID%in%age_pan_ID]

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%non_pan_preg_ID_age,]
  fwrite(my_preg_table,paste0(not_preg_folder,table_list[i]))
}

# HISTORICAL PREGNANT
# historical group entry/exit filter for observation time 2018-2020

age_hist_ID<-df_PERSONS$person_id[df_PERSONS$elligible_historical==1]
hist_preg_ID_age<-hist_preg_ID[hist_preg_ID%in%age_hist_ID]

for (i in 1:length(table_list)){
  my_table<-fread(paste0(preselect_folder,table_list[i]))
  my_preg_table<- my_table[my_table$person_id%in%hist_preg_ID_age,]
  fwrite(my_preg_table,paste0(hist_preg_folder,table_list[i]))
}
fwrite(my_PREG[my_PREG$person_id%in%hist_preg_ID_age,],paste0(hist_preg_folder,"my_PREG.csv"))


# #######################################################
# FLOWCHART to record attrition
#########################################################

flowchart<-as.data.frame(cbind(FC_OG_person_ID, FC_OG_preg, FC_OG_mom, FC_exclude_44, FC_preg_with_spell, FC_no_red_preg,
                               FC_sufficient_follow_up,   length(pan_preg_ID), length(pan_preg_ID_age),pan_PREGNANCIES_age,
                               length(hist_preg_ID),length(hist_preg_ID_age), length(between_preg_ID), FC_never_preg,length(unique(non_pan_preg_ID)), FC_all_non_pan_preg))
                         
 colnames(flowchart)<-c("All women of reproductive age between 2018-2021", "total pregnancies","total mothers", "pregnancies excluded due to gestation>44 weeks", "pregnancies with spell data","after excluding red pregnancies",
                        "pregnancies starting at least 12 months before end of study",   
                        "WOMEN with pandemic pregnancies","WOMEN with pandemic pregnancies with age 12-55 in 2020","number of included pandemic PREGNANCIES", "women with historical pregnancies",
                        "women with historical pregnancies with age 12-55 in 2018","women with between pregnancies", "women with no pregnancies EVER", "women with no pregnancy DURING pandemic", "women with no pregnancy DURING pandemic age 12-55 in 2020") 
 
 fwrite(flowchart, paste0(output_dir,"flowchart_study_pop.csv"))
 
 # before removing the preselect folder, copy cov_data out to projectFolder
 
 # cov_data<-fread(paste0(preselect_folder,"covid_data.csv"))
 
 # fwrite(cov_data, paste0(projectFolder, "/covid_data.csv"))
 
 #########################################################
 
 # delete preselect
 
 # unlink( paste0(projectFolder,"/CDMInstances_preselect"), recursive=T)
 
 