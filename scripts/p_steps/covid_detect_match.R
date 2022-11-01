#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 20/10/2022

library(tidyverse)

events_tables<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir = preselect_folder)

codesheet_cov<-fread(paste0(projectFolder,"/COVID_codes_19_10.csv"))
print(table(codesheet_cov$`Coding system`))

my_covid_codes<-codesheet_cov$Code

# DAPs EVENTS codes missing "." need to expand target list - across the covariates
no_dots_cov_codes <- my_covid_codes %>% str_replace_all('\\.', '')

all_covid_codes<-unique(c(my_covid_codes, no_dots_cov_codes))

my_rows<-which(Reduce(`|`, lapply(all_covid_codes, match, x = as.character(events_tables$event_code))))

person_id<-events_tables$person_id[my_rows]
cov_date<-events_tables$start_date_record[my_rows]
meaning<-events_tables$meaning_of_event[my_rows]
ev_code<-events_tables$event_code[my_rows]

table(ev_code)

print("total covid events")
print(length(person_id))

events_covid_data<-as.data.frame(cbind(person_id, cov_date, meaning, ev_code))
fwrite(events_covid_data, paste0(preselect_folder,"COVID_events_data.csv" ))

