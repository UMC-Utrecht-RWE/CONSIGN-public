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

covid_data<-covid_data[with(covid_data, order(person_id, cov_date)),]

covid_grouped<-covid_data%>%group_by(person_id)


first_covid<-covid_grouped%>%slice_head()
last_covid<-covid_grouped%>%slice_tail()

duration<-(last_covid$cov_date)-(first_covid$cov_date)
summary(duration)
hist(duration, breaks=30)
#GOOD this is what we expect

  single_episode_id<-first_covid$person_id[duration<31]
  multi_episode_id<-first_covid$person_id[duration>30]
  #check these are distinct cohorts
  all(single_episode_id%in%multi_episode_id==F)
  
  df_covid_single<-covid_data[covid_data$person_id%in%single_episode_id,]
  id_freq<-as.numeric(table(df_covid_single$person_id))
  
  df_covid_single$episode<-1
  
  df_covid_single<-df_covid_single[with(df_covid_single, order(person_id, severe)),]
  
  max_sev<-df_covid_single%>%group_by(person_id)%>%slice_tail()
  length(id_freq)==nrow(max_sev)
  df_covid_single$max_severity<-rep(max_sev$severe,id_freq)
  
  df_covid_multi<-covid_data[covid_data$person_id%in%multi_episode_id,]
  df_covid_multi<-df_covid_multi%>%group_by(person_id)
  # table(table(df_covid_multi$person_id))
  

#similar task to create_spells.... 

# FIRST need to group by person_id

# SECOND need to calculate ((each date)- (first_date))<=30 --> same episode else --> new episode

# THIRD for all in same episode, severity[i]=max(episode_severity)

covid_unique<- covid_data[(duplicated(covid_data[,1:2], fromLast = F)==F),]
covid_dup<- covid_data[(duplicated(covid_data[,1:2], fromLast = F)==T),]

#testing continuity of longitudinal data

fwrite(covid_unique, paste0(preselect_folder, "covid_data.csv"))
