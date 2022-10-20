#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 20/10/2022


events_tables<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir = preselect_folder)

codesheet_cov<-fread(paste0(projectFolder,"/COVID_codes_19_10.csv"))
print(table(codesheet_cov$`Coding system`))

my_covid_codes<-codesheet_cov$Code

my_rows<-which(Reduce(`|`, lapply(my_covid_codes, startsWith, x = as.character(events_tables$event_code))))

person_id<-events_tables$person_id[my_rows]
cov_date<-events_tables$start_date_record[my_rows]
meaning<-events_tables$meaning_of_event[my_rows]

print("total covid events")
print(length(person_id))

events_covid_data<-as.data.frame(cbind(person_id, cov_date, meaning))
fwrite(events_covid_data, paste0(preselect_folder,"COVID_events_data.csv" ))
