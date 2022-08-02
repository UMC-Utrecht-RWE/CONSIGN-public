#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#1/8/2022


#CONSIGN

# this script uses DAP specific COVID information to extract dates of COVID diagnoses

# user input can change to historical and non-pregnant
cohort_folder<- preselect_folder

CDM_source<-fread(paste0(path_CDM,"CDM_source.csv"))
DAP<-CDM_source$data_access_provider_name

# each DAP has 1)tables to read in 2)diagnosis column(s) 3)desired variable(s) value 4)date column 
DAP_names<-c("ARS", "FISABIO","SWANSEA","IACS","AARHUS", "BPE","KAROLINSKA", "UiO")

cov_dap_data<-as.data.frame(matrix(ncol = 2))
colnames(cov_dap_data)<-c("person_id", "cov_date")

if(DAP=="ARS"){
  my_data<-IMPORT_PATTERN(pat="SURVEY_ID", dir=cohort_folder)
  my_data<-my_data[(my_data$survey_meaning=='covid_registry'),]
  cov_dap_data$person_id<-my_data$person_id
  cov_dap_data$cov_date<-my_data$survey_date
}

if(DAP=="FISABIO"){
  my_data<-IMPORT_PATTERN(pat="MEDICAL_OBSERVATIONS", dir=cohort_folder)
  my_data1<-my_data[(my_data$mo_meaning=='covid19_pcr_test'& my_data$mo_source_value=="positive"),]
  my_data2<-my_data[(my_data$mo_meaning=='covid19_antigen_test'& my_data$mo_source_value=="positive"),]

  cov_dap_data$person_id<-c(my_data1$person_id, my_data2$person_id)
  cov_dap_data$cov_date<-c(my_data1$mo_date, my_data2$mo_date)
}


if(DAP=="SWANSEA"){
  my_data<-IMPORT_PATTERN(pat="MEDICAL_OBSERVATIONS", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='COVID_LIMS_TESTRESULTS'& my_data$mo_source_value==("ND6"|"ND7")),]
  
  cov_dap_data$person_id<-my_data$person_id
  cov_dap_data$cov_date<-my_data$survey_date
}

if(DAP=="IACS"){
  my_data<-IMPORT_PATTERN(pat="MEDICAL_OBSERVATIONS", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='COVID19 TEST'& my_data$mo_source_value==""),]
  
  cov_dap_data$person_id<-my_data$person_id
  cov_dap_data$cov_date<-my_data$survey_date
}


if(DAP=="Arhuis"){
  print("no DAP specific covid detection, only pfijzer algorithm")
}

if(DAP=="BPE"){
  print("no DAP specific covid detection, only pfijzer algorithm")
}


if(DAP=="Karolinska"){
  my_data<-IMPORT_PATTERN(pat="MEDICAL_OBSERVATIONS", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='covid_positive_test'& my_data$mo_source_value=="yes"),]
  
  cov_dap_data$person_id<-my_data$person_id
  cov_dap_data$cov_date<-my_data$survey_date
}


if(DAP=="UiO"){
  my_data<-IMPORT_PATTERN(pat="MEDICAL_OBSERVATIONS", dir=cohort_folder)
  my_data<-my_data[(my_data$mo_meaning=='covid19_test'& my_data$mo_source_value=="positive"),]
  
  cov_dap_data$person_id<-my_data$person_id
  cov_dap_data$cov_date<-my_data$survey_date
}

fwrite(cov_dap_data, paste0(cohort_folder,"COVID_data_dap.csv"))