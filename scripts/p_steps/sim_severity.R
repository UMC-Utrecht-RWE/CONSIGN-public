#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 8/8/2022


# combine Pjijzer standard COVID data with DAP tailored COVID data

# simulate covid severity (place holder until we decide how to determine severity)

covid_ev_data<-readRDS(paste0(preselect_folder , "I_COVID19DX_COV.rds"))


covid_ev_data<-select(.data = covid_ev_data, person_id, start_date_record)

covid_dap_data<-fread(paste0(preselect_folder,"COVID_data_dap.csv"))

colnames(covid_ev_data)<-colnames(covid_dap_data)

covid_data<-as.data.frame(rbind(covid_ev_data, covid_dap_data))

covid_data$severity<-sample(c("severe", "non_severe"), nrow(covid_data), replace = T)


fwrite(covid_data, paste0(preselect_folder, "covid_data.csv"))