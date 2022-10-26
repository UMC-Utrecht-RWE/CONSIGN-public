#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#4/8/2022

# CONSIGN
# this script will identify the target codes for each covariate
# covariates described in Teams file CONSIGN_Variables.xlsx using version 10/15/22


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name


cohort_folders<-list(preg_match_folder, cases_match_folder, cov_match_folder)
output_folders<-list(output_cov_neg_pan_preg, output_cov_pos_pan_preg, output_cov_pos_non_preg)


all_codes<-IMPORT_PATTERN(pat="codelist_CONSIGN", dir=projectFolder)

# only events within 1 year before covid+ pregnancy start date
# filter source data events everything before Jan 1 2019 (too old to be within covid preg window)

for(i in 1:length(cohort_folders)){
  
cohort_folder<-unlist(cohort_folders[i])

output_folder<-unlist(output_folders[i])

if(DAP!="Bordeaux"){
VACCINES<-IMPORT_PATTERN(pat="VACCINES", dir=cohort_folder)
EVENTS<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir=cohort_folder)
MED_OB<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
SURV_OB<-IMPORT_PATTERN(pat="SURVEY_SLIM", dir=cohort_folder)
MED<-IMPORT_PATTERN(pat="MEDICINES_SLIM", dir=cohort_folder)}else{
  VACCINES<-IMPORT_PATTERN(pat="VACCINES", dir=cohort_folder)
  EVENTS<-IMPORT_PATTERN(pat="EVENTS", dir=cohort_folder)
  MED_OB<-IMPORT_PATTERN(pat="MEDICAL_OB", dir=cohort_folder)
  SURV_OB<-IMPORT_PATTERN(pat="SURVEY_OB", dir=cohort_folder)
  MED<-IMPORT_PATTERN(pat="MEDICINES", dir=cohort_folder)
}

if(DAP!="USWAN"){MED$drug_date<-MED$date_dispensing}else{MED$drug_date<-MED$date_prescription}

#################################################################
#CARDIO

cardio_names<-c("C_CAD_AESI","C_CARDIOMYOPATHY_COV", "C_CMSTRESS_AESI","C_HF_COV","V_HYPERTENSION_COV")
cardio_codelist<-all_codes[all_codes$event_match_name%in%cardio_names,]
cardio_codelist$event_abbreviation<-"CARDIO"
CreateConceptDatasets(codesheet = cardio_codelist, fil=EVENTS, path = cov_comorbid_events)

CARDIO_EV<-readRDS(paste0(cov_comorbid_events, "CARDIO.rds"))

CARDIO_EV_ID<-(CARDIO_EV$person_id)
CARDIO_EV_Date<- (CARDIO_EV$start_date_record)

cardio_atc<-c("C01B","C01C","C01D","C01E", "B01A")

my_rows<-which(Reduce(`|`, lapply(cardio_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
CARDIO_MED_ID<-(MED$person_id[my_rows])
CARDIO_MED_Date<- (MED$drug_date[my_rows])

cardio_id<-c(CARDIO_EV_ID,  CARDIO_MED_ID)
cardio_date<-c(CARDIO_EV_Date, CARDIO_MED_Date)
cardio_cov<-as.data.frame(cbind(cardio_id, cardio_date))

fwrite(cardio_cov, paste0(output_folder,"cardio.csv"))

#################################################################################
# CANCER

cancer_names<-c("Onc_ANYMALIGNANCY_COV")
cancer_codelist<-all_codes[all_codes$event_match_name%in%cancer_names,]
cancer_codelist$event_abbreviation<-"cancer"
CreateConceptDatasets(codesheet = cancer_codelist, fil=EVENTS, path = cov_comorbid_events)

cancer_EV<-readRDS(paste0(cov_comorbid_events, "cancer.rds"))

cancer_EV_ID<-(cancer_EV$person_id)
cancer_EV_Date<- (cancer_EV$start_date_record)


cancer_atc<-c("L01A","L01B","L01C","L01D","L01X","L02A","L02B","L03","L04")

my_rows<-which(Reduce(`|`, lapply(cancer_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
cancer_MED_ID<-(MED$person_id[my_rows])
cancer_MED_Date<- (MED$drug_date[my_rows])

cancer_id<-c(cancer_EV_ID,  cancer_MED_ID)
cancer_date<-c(cancer_EV_Date, cancer_MED_Date)
cancer_cov<-as.data.frame(cbind(cancer_id, cancer_date))

fwrite(cancer_cov, paste0(output_folder,"cancer.csv"))

#################################################################################
# RESPIRATORY


resp_names<-c("R_RESPCHRONIC_COV")

resp_codelist<-all_codes[all_codes$event_match_name%in%resp_names,]
CreateConceptDatasets(codesheet = resp_codelist, fil=EVENTS, path = cov_comorbid_events)

resp_EV<-readRDS(paste0(cov_comorbid_events, "RESPCHRONIC.rds"))

resp_EV_ID<-(resp_EV$person_id)
resp_EV_Date<- (resp_EV$start_date_record)

resp_atc<-c("R03","R07AA","R07AB")

my_rows<-which(Reduce(`|`, lapply(resp_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
resp_MED_ID<-(MED$person_id[my_rows])
resp_MED_Date<- (MED$drug_date[my_rows])

resp_id<-c(resp_EV_ID, resp_MED_ID)
resp_date<-c(resp_EV_Date, resp_MED_Date)
resp_cov<-as.data.frame(cbind(resp_id, resp_date))

fwrite(resp_cov, paste0(output_folder,"respiratory.csv"))


#################################################################################
# LIVER


liver_names<-c("D_ALCOHOLICLIVER_COV" ,
               "D_HEPATITISAUTOIMMUNE_COV" ,
               "D_LIVERCHRONICALONE_COV" ,
               "D_MILDLIVERDISEASE_CH" ,
               "D_NONALCOHOLICLIVER_COV")
liver_codelist<-all_codes[all_codes$event_match_name%in%liver_names,]
liver_codelist$event_abbreviation<-"liver"
CreateConceptDatasets(codesheet = liver_codelist, fil=EVENTS, path = cov_comorbid_events)

liver_EV<-readRDS(paste0(cov_comorbid_events, "liver.rds"))

liver_EV_ID<-(liver_EV$person_id)
liver_EV_Date<- (liver_EV$start_date_record)


liver_atc<-c("J05AP")

my_rows<-which(Reduce(`|`, lapply(liver_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
liver_MED_ID<-(MED$person_id[my_rows])
liver_MED_Date<- (MED$drug_date[my_rows])

liver_id<-c(liver_EV_ID,  liver_MED_ID)
liver_date<-c(liver_EV_Date,  liver_MED_Date)
liver_cov<-as.data.frame(cbind(liver_id, liver_date))

fwrite(liver_cov, paste0(output_folder,"liver.csv"))
#################################################################################
# HIV


HIV_names<-c("I_HIVNOAIDS_CH")
HIV_codelist<-all_codes[all_codes$event_match_name%in%HIV_names,]
HIV_codelist$event_abbreviation<-"HIV"
CreateConceptDatasets(codesheet = HIV_codelist, fil=EVENTS, path = cov_comorbid_events)

HIV_EV<-readRDS(paste0(cov_comorbid_events, "HIV.rds"))

HIV_EV_ID<-(HIV_EV$person_id)
HIV_EV_Date<- (HIV_EV$start_date_record)

HIV_atc<-c("J05AE","J05AR", "J05AF","J05AG")

my_rows<-which(Reduce(`|`, lapply(HIV_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
HIV_MED_ID<-(MED$person_id[my_rows])
HIV_MED_Date<- (MED$drug_date[my_rows])

HIV_id<-c(HIV_EV_ID, HIV_MED_ID)
HIV_date<-c(HIV_EV_Date, HIV_MED_Date)
HIV_cov<-as.data.frame(cbind(HIV_id, HIV_date))

fwrite(HIV_cov, paste0(output_folder,"HIV.csv"))

#################################################################################
# KIDNEY


kidney_names<-c("G_KDCHRONIC_COV")
kidney_codelist<-all_codes[all_codes$event_match_name%in%kidney_names,]
kidney_codelist$event_abbreviation<-"kidney"
CreateConceptDatasets(codesheet = kidney_codelist, fil=EVENTS, path = cov_comorbid_events)

kidney_EV<-readRDS(paste0(cov_comorbid_events, "kidney.rds"))

kidney_EV_ID<-(kidney_EV$person_id)
kidney_EV_Date<- (kidney_EV$start_date_record)


kidney_atc<-c("B03XA01")

my_rows<-which(Reduce(`|`, lapply(kidney_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
kidney_MED_ID<-(MED$person_id[my_rows])
kidney_MED_Date<- (MED$drug_date[my_rows])

kidney_id<-c(kidney_EV_ID,  kidney_MED_ID)
kidney_date<-c(kidney_EV_Date, kidney_MED_Date)
kidney_cov<-as.data.frame(cbind(kidney_id, kidney_date))

fwrite(kidney_cov, paste0(output_folder,"kidney.csv"))

#################################################################################
# DIABETES


diabetes_names<-c("E_DM12_COV")
diabetes_codelist<-all_codes[all_codes$event_match_name%in%diabetes_names,]
diabetes_codelist$event_abbreviation<-"diabetes"
CreateConceptDatasets(codesheet = diabetes_codelist, fil=EVENTS, path = cov_comorbid_events)

diabetes_EV<-readRDS(paste0(cov_comorbid_events, "diabetes.rds"))

diabetes_EV_ID<-(diabetes_EV$person_id)
diabetes_EV_Date<- (diabetes_EV$start_date_record)
diabetes_atc<-c("A10B","A10A")

my_rows<-which(Reduce(`|`, lapply(diabetes_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
diabetes_MED_ID<-(MED$person_id[my_rows])
diabetes_MED_Date<- (MED$drug_date[my_rows])

diabetes_id<-c(diabetes_EV_ID,  diabetes_MED_ID)
diabetes_date<-c(diabetes_EV_Date, diabetes_MED_Date)
diabetes_cov<-as.data.frame(cbind(diabetes_id, diabetes_date))

fwrite(diabetes_cov, paste0(output_folder,"diabetes.csv"))

#################################################################################
# OBESITY

# ARS so_source_table = "CAP1", so source_column = "ALTEZZA" is height in centimeters, so_source_table = "CAP1", so source_column = "PESO" is weight in kg
# USWAN so_source_table='mids_initial_assessment', so_source_column='BMI', so_source_value  "=> 30"
# IACS so_source_column='BMI', so_source_value "=> 30"
# aarhus so_source_column='BMI_MODER' AND so_source_value="=> 30"
# karolinska so_source_column='BMI vid inskrivning' AND so_source_value="=> 30"
# UOSL so_source_column='KMI_foer' AND so_source_value="=> 30"
# --> decided to skip calculated BMI 24/10

obesity_names<-c("L_OBESITY_COV")
obesity_codelist<-all_codes[all_codes$event_match_name%in%obesity_names,]
obesity_codelist$event_abbreviation<-"obesity"
CreateConceptDatasets(codesheet = obesity_codelist, fil=EVENTS, path = cov_comorbid_events)

obesity_EV<-readRDS(paste0(cov_comorbid_events, "obesity.rds"))

obesity_EV_ID<-(obesity_EV$person_id)
obesity_EV_Date<- (obesity_EV$start_date_record)


obesity_atc<-c("A08AB","A08AA")

my_rows<-which(Reduce(`|`, lapply(obesity_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
obesity_MED_ID<-(MED$person_id[my_rows])
obesity_MED_Date<- (MED$drug_date[my_rows]) 

obesity_id<-c(obesity_EV_ID, obesity_MO_ID, obesity_SO_ID, obesity_MED_ID)
obesity_date<-c(obesity_EV_Date, obesity_MO_Date, obesity_SO_Date, obesity_MED_Date)
obesity_cov<-as.data.frame(cbind(obesity_id, obesity_date))

fwrite(obesity_cov, paste0(output_folder,"obesity.csv"))

#################################################################################
# SICLECELL


sickle_names<-c("B_SICKLECELL_COV")
sickle_codelist<-all_codes[all_codes$event_match_name%in%sickle_names,]
sickle_codelist$event_abbreviation<-"sickle"
CreateConceptDatasets(codesheet = sickle_codelist, fil=EVENTS, path = cov_comorbid_events)

sickle_EV<-readRDS(paste0(cov_comorbid_events, "sickle.rds"))

sickle_EV_ID<-(sickle_EV$person_id)
sickle_EV_Date<- (sickle_EV$start_date_record)

sickle_atc<-c("L01XX05","B06AX")

my_rows<-which(Reduce(`|`, lapply(sickle_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
sickle_MED_ID<-(MED$person_id[my_rows])
sickle_MED_Date<- (MED$drug_date[my_rows])

sickle_id<-c(sickle_EV_ID,  sickle_MED_ID)
sickle_date<-c(sickle_EV_Date,  sickle_MED_Date)
sickle_cov<-as.data.frame(cbind(sickle_id, sickle_date))

fwrite(sickle_cov, paste0(output_folder,"sickle.csv"))

#################################################################################
# IMMUNOSUPRESSION


immunosupress_names<-c("Im_TRANSPLANTRECIPIENT_COV")
immunosupress_codelist<-all_codes[all_codes$event_match_name%in%immunosupress_names,]
immunosupress_codelist$event_abbreviation<-"immunosupress"
CreateConceptDatasets(codesheet = immunosupress_codelist, fil=EVENTS, path = cov_comorbid_events)

immunosupress_EV<-readRDS(paste0(cov_comorbid_events, "immunosupress.rds"))

immunosupress_EV_ID<-(immunosupress_EV$person_id)
immunosupress_EV_Date<- (immunosupress_EV$start_date_record)

immsup_atc<-c("L04A","H02")

my_rows<-which(Reduce(`|`, lapply(immsup_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
immsup_MED_ID<-(MED$person_id[my_rows])
immsup_MED_Date<- (MED$drug_date[my_rows])

immsup_id<-c(immsup_EV_ID,  immsup_MED_ID)
immsup_date<-c(immsup_EV_Date, immsup_MED_Date)
immsup_cov<-as.data.frame(cbind(immsup_id, immsup_date))

fwrite(immsup_cov, paste0(output_folder,"immunosupression.csv"))

#################################################################################
# MENTAL

# medicinal_product_atc_code=code in "DP_COVMENTALHEALTH"
# Date is "date_prescription" or "date_dispensing"

mental_atc<-c("N05A", "N05B", "N06A")

my_rows<-which(Reduce(`|`, lapply(mental_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))

mental_id<-(MED$person_id[my_rows])
mental_date<- (MED$drug_date[my_rows])
                   
mental_cov<-as.data.frame(cbind(mental_id, mental_date))

fwrite(mental_cov, paste0(output_folder,"mental.csv"))


#################################################################################
# VACCINE

if(DAP!="ARS"){
vaccine_atc<-("J07BB")

my_rows<-which(Reduce(`|`, lapply(vaccine_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))

vaccine_MED_id<-(MED$person_id[my_rows])
vaccine_MED_date<- (MED$drug_date[my_rows])}else{
vaccine_MED_id<-NA
vaccine_MED_date<-NA}

if(DAP%in%c("ARS","FISABIO", "USWAN","UOSL")){

my_rows<-which(Reduce(`|`, lapply(vaccine_atc, startsWith, x = as.character(VACCINES$vx_atc))))

vaccine_VAC_id<-(VACCINES$person_id[my_rows])
vaccine_VAC_date<- (VACCINES$vx_admin_date[my_rows])}else{
  vaccine_VAC_id<-NA
  vaccine_VAC_date<-NA}

if(DAP=="Bordeaux"){
  my_rows<-which(Reduce(`|`, lapply(vaccine_atc, startsWith, x = as.character(VACCINES$vx_atc))))
  
  vaccine_VAC_id<-(VACCINES$person_id[my_rows])
  vaccine_VAC_date<- (VACCINES$vx_record_date[my_rows])}
  
 
vaccine_id<-c(vaccine_MED_id, vaccine_VAC_id)
vaccine_date<-c(vaccine_MED_date, vaccine_VAC_date)

vaccine_cov<-as.data.frame(cbind(vaccine_id, vaccine_date))

fwrite(vaccine_cov, paste0(output_folder,"vaccine.csv"))

}
