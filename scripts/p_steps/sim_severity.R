#simulate covid severity (place holder until we decide how to determine severity)

covid_data<-readRDS(paste0(path_CDM , "I_COVID19DX_COV.rds"))

summary(covid_data)
covid_data<-select(.data = covid_data, person_id, start_date_record)

covid_data$severity<-sample(c("severe", "non_severe"), nrow(covid_data), replace = T)

colnames(covid_data)<-c("person_id", "cov_date", "severity")

saveRDS(covid_data, paste0(path_CDM, "covid_data.rds"))