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
# merge covid episodes


# need to match by person id and date

#exact date? 

covid_unique<- covid_data[(duplicated(covid_data[,1:2], fromLast = F)==F),]
covid_dup<- covid_data[(duplicated(covid_data[,1:2], fromLast = F)==T),]

# 
# table(covid_dup$person_id%in%covid_unique$person_id)
# table(table(covid_dup$person_id))
# table(table(covid_data$person_id))
# table(table(covid_unique$person_id))

# covid unique has all records with the same ID & same Date removed 
# person_id multiples have unique dates

#death? EImir input (assume )
#severity indicators from TEAMS sheet, looking for ANY match, so doesn't need to be DAP specific

severity_indicators<-c("hospital", "pd", "sd", "intensive", "cause_of_death")

sev_rows<-which(Reduce(`|`, lapply(severity_indicators, startsWith, x = as.character(covid_unique$meaning))))

covid_unique$severe<-0

covid_unique$severe[sev_rows]<-1

# need to group events --> episodes (any severe indicator with 4 weeks of first date)
# AAAAAHHHHHHHHHHHH >_<
# create "episode" varaible

# table(covid_unique$severity)


#labor (end of pregnancy) (cov_date-end_of_pregnancy == [-3:+3])--> subset
#check Rosa input on timeframe symmetry
# sequester these cases at a later stage
# eimir 9/9 2 days before end_of_preg, from OBS
# remove cov+preg days to end_of_preg<2 (sandbox the labor/cov confounded group)


fwrite(covid_unique, paste0(preselect_folder, "covid_data.csv"))
