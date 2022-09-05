#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/8/2022


# combine Pjijzer standard COVID data with DAP tailored COVID data


# for each COVID infection (possibly multiple dates), either severe OR non-severe
#first date --> look forward 4 weeks for severity

# simulate covid severity (place holder until we decide how to determine severity)

#check duplicates
# IF same person, same date duplicates but incongruent severity--> severe

covid_ev_data<-readRDS(paste0(preselect_folder , "I_COVID19DX_COV.rds"))

covid_ev_data<-select(.data = covid_ev_data, person_id, start_date_record, meaning_of_event)

#teams variables document: "hospital" "intensive" "pd / sd" (whales)(careful for captial and lower case)


covid_dap_data<-fread(paste0(preselect_folder,"COVID_data_dap.csv"))

colnames(covid_ev_data)<-colnames(covid_dap_data)

covid_data<-as.data.frame(rbind(covid_ev_data, covid_dap_data))

table(covid_data$meaning)

#death? EImir input

#labor (end of pregnancy) (cov_date-end_of_pregnancy == [-3:+3])--> subset

covid_data$severity<-sample(c("severe", "non_severe"), nrow(covid_data), replace = T)


fwrite(covid_data, paste0(preselect_folder, "covid_data.csv"))
