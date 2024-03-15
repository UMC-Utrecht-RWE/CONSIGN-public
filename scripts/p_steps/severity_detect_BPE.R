#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/8/2022


# combine Pjijzer standard COVID data with DAP tailored COVID data



cov_ev_data<-IMPORT_PATTERN(pat="covid19_diagnosis_",colls=c("person_id", "start_date_record", "meaning_of_event"), dir=preselect_folder)

colnames(cov_ev_data)<-c("person_id", "cov_date", "meaning")

covid_data<-cov_ev_data


# remove rows with missing data
covid_data<-covid_data[complete.cases(covid_data),]

covid_data$severe<-1

fwrite(covid_data, paste0(preselect_folder, "/covid_data.csv"))