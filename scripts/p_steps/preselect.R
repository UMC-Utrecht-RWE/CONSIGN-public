#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 10/6/2022

#this script is the first step in creating the CONSIGN study populations.
#1)identifies person_ids with elligible OBS time (2018-present)
# 2)identifies women of reproductive age 
#  3)filters out only those IDs that meet the OBS, gender and age criteria and subsets the CDM


if(!require(data.table)){install.packages("data.table")}
library(data.table)

#set up folders
preselect_folder<-paste0(projectFolder,"/CDMInstances_preselect/")
#make sure it's empty
do.call(file.remove, list(list.files(preselect_folder, full.names = TRUE)))

study_start_date<-as.Date("20180101", format="%Y%m%d")

OBSERVATION_PERIODS <- fread(paste0(path_CDM, "OBSERVATION_PERIODS.csv"))
OBSERVATION_PERIODS$date_end<-as.Date(as.character(OBSERVATION_PERIODS$op_end_date), format = "%Y%m%d")

OB_P_ID<-unique(OBSERVATION_PERIODS$person_id[OBSERVATION_PERIODS$date_end>=study_start_date])

#preselection application onto multiple table subsets (especially MEDICINES)

#get tables 
#Get EVENTS, MO, SO, MEDICINES, VACCINES tables
actual_tables_preselect<-list()
actual_tables_preselect$EVENTS<-list.files(path_CDM, pattern="^EVENTS")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(path_CDM, pattern="^MEDICAL_OBSERVATIONS")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(path_CDM, pattern="^SURVEY_OBSERVATIONS")
actual_tables_preselect$MEDICINES<-list.files(path_CDM, pattern="^MEDICINES")
actual_tables_preselect$VACCINES<-list.files(path_CDM, pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(path_CDM, pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(path_CDM, pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(path_CDM, pattern="^PERSONS")

all_actual_tables<-list.files(path_CDM, pattern = "\\.csv$")

##############
#get functions
##############

#firstflter function selects females within age range from the persons table and stores the selected IDs to use for subsequent filtering
personsfilter<-function(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=1954, dobmax=2008) {
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



##############################################################
#run personsfilter on PERSONS table (PERSONS USUALLY* one table)


PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir = path_CDM)

personsfilter_output<-as.vector((personsfilter(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=1954, dobmax=2008)))
personsfilter_ID<-personsfilter_output[[1]]


OB_PERS_filter_ID<-personsfilter_ID[personsfilter_ID%in%OB_P_ID]

# #write preselected files into new folder
# 
person_preselect_tables<-actual_tables_preselect
# [names(actual_tables_preselect) %in% "MEDICINES" == FALSE]
tables_df<-as.data.frame(unlist(person_preselect_tables))
colnames(tables_df)<-"CDMtableName"
tables_vec_all<-unique(as.vector(tables_df$CDMtableName))

#subset data using preselection IDs and write new files
#need to name each new table the same as the old table, then write in the new folder
for(i in 1:length(tables_vec_all)){
  tablename<-(tables_vec_all[i])
  mytable<-fread(paste0(path_CDM,tablename))
  preselect_table<-mytable[mytable$person_id%in%OB_PERS_filter_ID,]
  fwrite(preselect_table, paste0(preselect_folder, tablename), row.names = F)
}

actual_tables_preselect_changed<-list.files(preselect_folder, pattern = "\\.csv$")
to_be_copied<-setdiff(all_actual_tables,actual_tables_preselect_changed)

for(fil_ind in 1:length(to_be_copied)){
  tablename<-(to_be_copied[fil_ind])
  mytable<-fread(paste0(path_CDM,tablename))
  fwrite(mytable, paste0(preselect_folder, tablename), row.names = F)
}

