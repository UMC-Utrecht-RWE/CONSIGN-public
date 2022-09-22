#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#22/9/2022


# restrict dates of MEDICINES, EVENTS, SURVEY OBS AND MED OBS

start_study_date<-as.numeric(as.Date(as.character("20180101"), format = "%Y%m%d"))

my_dt_MED<-IMPORT_PATTERN(pat="MEDICINES", dir = preselect_folder)

df <- select(my_dt_MED, date_dispensing, date_prescription)

drug_date<-df %>% transmute(Label = coalesce(date_dispensing, date_prescription))

my_dt_MED$drug_date<-drug_date

my_dt_MED$drug_date<-as.Date(my_dt_MED$drug_date, format="%Y%m%d")

my_dt_MED$drug_date<-as.numeric(my_dt_MED$drug_date)

my_dt_MED<-my_dt_MED[is.na(my_dt_MED$drug_date)==F,]

my_dt_MED<-my_dt_MED[my_dt_MED$drug_date>=start_study_date,]

fwrite(my_dt_MED,paste0( preselect_folder, "MEDICINES_SLIM.csv"))
 
#########################################################################

my_dt_SURV<-IMPORT_PATTERN(pat="SURVEY", dir = preselect_folder)

my_dt_SURV$so_date<-as.Date(my_dt_SURV$so_date, format="%Y%m%d")

my_dt_SURV$so_date<-as.numeric(my_dt_SURV$so_date)

my_dt_SURV<-my_dt_SURV[is.na(my_dt_SURV$so_date)==F,]

my_dt_SURV<-my_dt_SURV[my_dt_SURV$so_date>=start_study_date,]

fwrite(my_dt_SURV,paste0( preselect_folder, "SURVEY_SLIM.csv"))

#########################################################################

my_dt_MO<-IMPORT_PATTERN(pat="MEDICAL", dir = preselect_folder)

my_dt_MO$mo_date<-as.Date(my_dt_MO$mo_date, format="%Y%m%d")

my_dt_MO$mo_date<-as.numeric(my_dt_MO$mo_date)

my_dt_MO<-my_dt_MO[is.na(my_dt_MO$mo_date)==F,]

my_dt_MO<-my_dt_MO[my_dt_MO$mo_date>=start_study_date,]

fwrite(my_dt_MO,paste0( preselect_folder, "MED_OB_SLIM.csv"))

#########################################################################

my_dt_EV<-IMPORT_PATTERN(pat="EVENT", dir = preselect_folder)

my_dt_EV$start_date_record<-as.Date(my_dt_EV$start_date_record, format="%Y%m%d")

my_dt_EV$start_date_record<-as.numeric(my_dt_EV$start_date_record)

my_dt_EV$end_date_record<-as.Date(my_dt_EV$end_date_record, format="%Y%m%d")

my_dt_EV$end_date_record<-as.numeric(my_dt_EV$end_date_record)


my_dt_EV<-my_dt_EV[is.na(my_dt_EV$start_date_record)==F,]

my_dt_EV<-my_dt_EV[my_dt_EV$start_date_record>=start_study_date,]

fwrite(my_dt_EV,paste0( preselect_folder, "EVENTS_SLIM.csv"))


#########################################################################
