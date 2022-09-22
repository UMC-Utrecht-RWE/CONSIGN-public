#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#22/9/2022


# restrict dates of MEDICINES, EVENTS, SURVEY OBS AND MED OBS

start_covariate_window<-as.numeric(as.Date(as.character("20190101"), format = "%Y%m%d"))

my_dt_MED<-IMPORT_PATTERN(pat="MEDICINES", dir = preselect_folder)

drug_date<-df %>% transmute(Label = coalesce(date_dispensing, date_prescription))

my_dt_MED$drug_date<-drug_date

my_dt_MED$drug_date<-as.Date(my_dt_MED$drug_date, format="%Y%m%d")

my_dt_MED$drug_date<-as.numeric(my_dt_MED$drug_date)

my_dt_MED<-my_dt_MED[is.na(my_dt_MED$drug_date)==F,]

my_dt_MED<-my_dt_MED[my_dt_MED$drug_date>=start_covariate_window,]

fwrite(my_dt_MED,paste0( preselect_folder, "MEDICINES_SLIM.csv"))
       