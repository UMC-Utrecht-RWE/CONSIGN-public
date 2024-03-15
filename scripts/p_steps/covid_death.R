#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#15/10/2022

# consign

# final severity check: death within 28 days of cov_date

covid_data<-fread(paste0(preselect_folder, "covid_data.csv"))

PERSONS<-fread(paste0(preselect_folder,"PERSONS.csv"))

PERSONS_with_covid_DOD<-PERSONS$num_DOD[PERSONS$person_id%in%covid_data$person_id]

cov_id_freq<-as.numeric(table(covid_data$person_id))

days_between_cov_date_DOD<-(rep(PERSONS_with_covid_DOD,cov_id_freq))-covid_data$cov_date

# summary(days_between_cov_date_DOD)
covid_data$cov_death<-0
covid_data$cov_death[between(days_between_cov_date_DOD,0,28)]<-1
covid_data$mo_severe<-covid_data$severe

covid_data$severe<-(covid_data$mo_severe+covid_data$cov_death)

covid_data$severe[covid_data$severe>0]<-1


print("check that reasonable proportion of covid cases are severe")
print(table(covid_data$severe))

fwrite (covid_data, paste0(preselect_folder,"covid_data.csv"))
