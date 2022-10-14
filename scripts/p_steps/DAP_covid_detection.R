#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#1/8/2022


#CONSIGN

# this script uses DAP specific COVID information to extract dates of COVID diagnoses

# user input can change to historical and non-pregnant
cohort_folder<- preselect_folder

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

# each DAP has 1)tables to read in 2)diagnosis column(s) 3)desired variable(s) value 4)date column 
DAP_names<-c("ARS", "FISABIO","SWANSEA","IACS","ARHUS", "BPE","KAROLINSKA", "UOSL")

cov_date<-vector()
person_id<-vector()
meaning<-vector()

if(DAP=="ARS"){
  my_data<-IMPORT_PATTERN(pat="SURVEY_SLIM", dir=cohort_folder)
  my_data<-my_data[(my_data$so_meaning=='covid_registry'),]
  person_id<-my_data$person_id
  cov_date<-my_data$survey_date
  meaning<-my_data$survey_meaning
}

if(DAP=="FISABIO"){
  my_data<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
  my_data1<-my_data[(my_data$mo_meaning=='covid19_pcr_test'& my_data$mo_source_value=="positive"),]
  my_data2<-my_data[(my_data$mo_meaning=='covid19_antigen_test'& my_data$mo_source_value=="positive"),]

  person_id<-c(my_data1$person_id, my_data2$person_id)
  cov_date<-c(my_data1$mo_date, my_data2$mo_date)
  meaning<-my_data$mo_meaning
}


if(DAP=="USWAN"){
  my_data<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='COVID_LIMS_TESTRESULTS'& my_data$mo_source_value==("ND6"|"ND7")),]
  
  person_id<-my_data$person_id
  cov_date<-my_data$mo_date
  meaning<-my_data$mo_meaning
}

# URGENT check in teams doc
if(DAP=="IACS"){
  my_data<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='covid19_pcr_test'& my_data$mo_source_value==""),]
  
  person_id<-my_data$person_id
  cov_date<-my_data$mo_date
  meaning<-my_data$mo_meaning
}


if(DAP=="Aarhus"){
  print("no DAP specific covid detection, only pfijzer algorithm")
}

if(DAP=="Bordeaux"){
  print("no DAP specific covid detection, only pfijzer algorithm")
}


if(DAP=="Karolinska Institutet"){
  my_data<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='covid_positive_test'& my_data$mo_source_value=="yes"),]
  
  person_id<-my_data$person_id
  cov_date<-my_data$mo_date
  meaning<-my_data$mo_meaning
}


if(DAP=="UOSL"){
  my_data<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='covid19_test'& my_data$mo_source_value=="positive"),]
  
  person_id<-my_data$person_id
  cov_date<-my_data$mo_date
  meaning<-my_data$mo_meaning
}

if(DAP=="TEST"){
  my_data<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
  my_data1<-my_data[(my_data$mo_meaning=='covid19_pcr_test'& my_data$mo_source_value=="positive"),]
  my_data2<-my_data[(my_data$mo_meaning=='covid19_antigen_test'& my_data$mo_source_value=="positive"),]
  
  person_id<-c(my_data1$person_id, my_data2$person_id)
  cov_date<-c(my_data1$mo_date, my_data2$mo_date)
  meaning<-c(my_data1$mo_meaning, my_data2$mo_meaning)
}

###################################################


cov_dap_data<-as.data.frame(cbind(person_id, cov_date, meaning))

fwrite(cov_dap_data, paste0(cohort_folder,"COVID_data_dap.csv"))
