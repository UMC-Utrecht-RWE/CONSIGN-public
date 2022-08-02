#simulate covid severity (place holder until we decide how to determine severity)

covid_ev_data<-readRDS(paste0(preselect_folder , "I_COVID19DX_COV.rds"))


covid_ev_data<-select(.data = covid_ev_data, person_id, start_date_record)

covid_dap_data<-fread(paste0(preselect_folder,"COVID_data_dap.csv"))

covid_data<-as.data.frame(rbind(covid_ev_data, covid_dap_data))

covid_data$severity<-sample(c("severe", "non_severe"), nrow(covid_data), replace = T)

colnames(covid_data)<-c("person_id", "cov_date", "severity")

fwrite(covid_data, paste0(preselect_folder, "covid_data.csv"))