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

sev_rows<-which(Reduce(`|`, lapply(severity_indicators, startsWith, x = as.character(covid_unique$meaning))))

covid_unique$severe<-0

covid_unique$severe[sev_rows]<-1

# need to group events --> episodes (any severe indicator with 4 weeks of first date)
# AAAAAHHHHHHHHHHHH >_<
# create "episode" varaible

# merge covid episodes

table(table(covid_data$person_id))

#similar task to create_spells.... 

# FIRST need to group by person_id

# SECOND need to calculate ((each date)- (first_date))<=30 --> same episode else --> new episode

# THIRD for all in same episode, severity[i]=max(episode_severity)

covid_unique<- covid_data[(duplicated(covid_data[,1:2], fromLast = F)==F),]
covid_dup<- covid_data[(duplicated(covid_data[,1:2], fromLast = F)==T),]

#testing continuity of longitudinal data

fwrite(covid_unique, paste0(preselect_folder, "covid_data.csv"))
