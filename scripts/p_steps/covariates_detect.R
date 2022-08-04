#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#4/8/2022

# CONSIGN
# this script will identify the target codes for each covariate


EVENTS<-IMPORT_PATTERN(pat="EVENTS", dir=cohort_folder)
MED_OB<-IMPORT_PATTERN(pat="MEDICAL_OB", dir=cohort_folder)
SURV_OB<-IMPORT_PATTERN(pat="SURVEY_OB", dir=cohort_folder)
MED<-IMPORT_PATTERN(pat="MEDICINES", dir=cohort_folder)


all_codes<-fread(paste0(projectFolder,"/ALL_full_codelist.csv"))

#################################################################

cardio_names<-c("C_CAD_AESI","C_CARDIOMYOPATHY_COV", "C_CMSTRESS_AESI","C_HF_COV","V_HYPERTENSION_COV")
my_rows<-which(Reduce(`|`, lapply(cardio_names, startsWith, x = as.character(all_codes$full_name))))

cardio_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(cardio_codes, startsWith, x = as.character(EVENTS$event_code))))
CARDIO_EV_ID<-(EVENTS$person_id[my_rows])
CARDIO_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(cardio_codes, startsWith, x = as.character(MED_OB$mo_code))))
CARDIO_MO_ID<-(MED_OB$person_id[my_rows])
CARDIO_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(cardio_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
CARDIO_SO_ID<-(SURV_OB$person_id[my_rows])
CARDIO_SO_Date<- (SURV_OB$so_date[my_rows])

cardio_atc<-c("C01B","C01C","C01D","C01E", "B01A")

my_rows<-which(Reduce(`|`, lapply(cardio_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
CARDIO_MED_ID<-(MED$person_id[my_rows])
CARDIO_MED_Date<- (MED$so_date[my_rows])

cardio_id<-c(CARDIO_EV_ID, CARDIO_MO_ID, CARDIO_SO_ID, CARDIO_MED_ID)
cardio_date<-c(CARDIO_EV_Date, CARDIO_MO_Date, CARDIO_SO_Date, CARDIO_MED_Date)
cardio_cov<-as.data.frame(cbind(cardio_id, cardio_date))

fwrite(cardio_cov, paste0(output_cov,"cardio.csv"))

#################################################################################