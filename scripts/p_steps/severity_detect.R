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

# remove rows with missing data
covid_data<-covid_data[complete.cases(covid_data),]
# fix date format
covid_data$cov_date<-as.Date(covid_data$cov_date, format = "%Y%m%d")
covid_data$cov_date<-as.numeric(covid_data$cov_date)

#severity indicators from TEAMS sheet, looking for ANY match, so doesn't need to be DAP specific

severity_indicators<-c("hospital", "pd", "sd", "intensive", "cause_of_death")

# will STARTSWITH become a problem? rather look for string at any position

sev_rows<-which(Reduce(`|`, lapply(severity_indicators, startsWith, x = as.character(covid_data$meaning))))

covid_data$severe<-0

covid_data$severe[sev_rows]<-1

# #only for development!!!
# # the simulated RTI data has date values that make no sense (before COVID existed... so I'm going to impute dates that will make sense)
# 
# cov_start<- as.numeric(as.Date("2020-01-01", format="%Y-%m-%d"))
# covid_data$cov_date<-cov_start
# episode_duration<-sample(1:30, nrow(covid_data), replace = T)+sample(c(0,200),nrow(covid_data), replace = T)
# covid_data$cov_date<-covid_data$cov_date+episode_duration
# 
# covid_data$severe<-sample(0:1, nrow(covid_data), replace = T)
# 

# need to group events --> episodes (any severe indicator with 4 weeks of first date)
# AAAAAHHHHHHHHHHHH >_<
# create "episode" varaible

# merge covid episodes

# table(table(covid_data$person_id))
# 
# covid_data<-covid_data[with(covid_data, order(person_id, cov_date)),]
# id_freq<-as.numeric(table(covid_data$person_id))
# covid_grouped<-covid_data%>%group_by(person_id)
# 
# 
# first_covid<-covid_grouped%>%slice_head()
# covid_data$duration<-covid_data$cov_date-(rep(first_covid$cov_date, id_freq))
# 
# hist(covid_data$duration, breaks=30)
#GOOD this is what we expect

  
  df_covid_first<-covid_data[covid_data$duration<29,]
  df_covid_first$episode<-1
  
  df_covid_first<-df_covid_first[with(df_covid_first, order(person_id, severe)),]
  id_freq<-as.numeric(table(df_covid_first$person_id))
  
  max_sev<-df_covid_first%>%group_by(person_id)%>%slice_tail()
  
  length(id_freq)==nrow(max_sev)
  df_covid_first$max_severity<-rep(max_sev$severe,id_freq)
  
  
#get duration of subsequent cov_dates  
  df_covid_multi<-covid_data[covid_data$duration>28,]
  df_covid_multi<-df_covid_multi%>%group_by(person_id)
  
  id_freq<-as.numeric(table(df_covid_multi$person_id))
  first_covid<-df_covid_multi%>%slice_head()
  df_covid_multi$duration_2<-df_covid_multi$cov_date-(rep(first_covid$cov_date, id_freq))
  
  
  
  df_covid_second<-df_covid_multi[df_covid_multi$duration_2<29,]
  df_covid_second$episode<-2
  df_covid_second<-df_covid_second[with(df_covid_second, order(person_id, severe)),]
  id_freq<-as.numeric(table(df_covid_second$person_id))
  max_sev<-df_covid_second%>%group_by(person_id)%>%slice_tail()
  length(id_freq)==nrow(max_sev)
  df_covid_second$max_severity<-rep(max_sev$severe,id_freq)
  
  
  
  df_covid_third<-df_covid_multi[df_covid_multi$duration_2>28,]
  df_covid_third$episode<-3
  df_covid_third<-df_covid_third[with(df_covid_third, order(person_id, severe)),]
  id_freq<-as.numeric(table(df_covid_third$person_id))
  max_sev<-df_covid_third%>%group_by(person_id)%>%slice_tail()
 
  df_covid_third$max_severity<-rep(max_sev$severe,id_freq)

  
  person_id<-c(df_covid_first$person_id, df_covid_second$person_id, df_covid_third$person_id)  
  covid_date<-c(df_covid_first$cov_date, df_covid_second$cov_date, df_covid_third$cov_date) 
  episode<-c(df_covid_first$episode, df_covid_second$episode, df_covid_third$episode) 
  severe<-c(df_covid_first$max_severity, df_covid_second$max_severity, df_covid_third$max_severity) 
  
  final_covid_data<-as.data.frame(cbind(person_id, covid_date, episode, severe))
  final_covid_data<-final_covid_data[with(final_covid_data, order(person_id, covid_date)),]
  nrow(final_covid_data)==nrow(covid_data)

fwrite(final_covid_data, paste0(preselect_folder, "covid_data.csv"))
