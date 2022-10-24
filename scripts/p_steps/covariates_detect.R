#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#4/8/2022

# CONSIGN
# this script will identify the target codes for each covariate
# covariates described in Teams file CONSIGN_Variables.xlsx using version 10/15/22


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name


cohort_folders<-list(preg_match_folder, cov_pos_pan_preg_folder, cov_match_folder, hist_preg_folder )
output_folders<-list(output_cov_neg_pan_preg, output_cov_pos_pan_preg, output_cov_pos_non_preg, output_hist_preg)

all_codes<-fread(paste0(projectFolder,"/ALL_full_codelist.csv"))
my_codes<-all_codes$code
no_dots_codes <- my_codes %>% str_replace_all('\\.', '')
all_codes$code_no_dots<-no_dots_codes

# only events within 1 year before covid+ pregnancy start date
# filter source data events everything before Jan 1 2019 (too old to be within covid preg window)

for(i in 1:length(cohort_folders)){
  
cohort_folder<-unlist(cohort_folders[i])

output_folder<-unlist(output_folders[i])

VACCINES<-IMPORT_PATTERN(pat="VACCINES", dir=cohort_folder)
EVENTS<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir=cohort_folder)
MED_OB<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
SURV_OB<-IMPORT_PATTERN(pat="SURVEY_SLIM", dir=cohort_folder)
MED<-IMPORT_PATTERN(pat="MEDICINES_SLIM", dir=cohort_folder)

if(DAP!="USWAN"){MED$drug_date<-MED$date_dispensing}else{MED$drug_date<-MED$date_prescription}

#################################################################
#CARDIO

cardio_names<-c("C_CAD_AESI","C_CARDIOMYOPATHY_COV", "C_CMSTRESS_AESI","C_HF_COV","V_HYPERTENSION_COV")
my_rows<-which(Reduce(`|`, lapply(cardio_names, startsWith, x = as.character(all_codes$full_name))))

cardio_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(cardio_codes, match, x = as.character(EVENTS$event_code))))
CARDIO_EV_ID<-(EVENTS$person_id[my_rows])
CARDIO_EV_Date<- (EVENTS$start_date_record[my_rows])

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
my_rows<-which(Reduce(`|`, lapply(cancer_names, startsWith, x = as.character(all_codes$full_name))))

cancer_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(cancer_codes, match, x = as.character(EVENTS$event_code))))
cancer_EV_ID<-(EVENTS$person_id[my_rows])
cancer_EV_Date<- (EVENTS$start_date_record[my_rows])



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
my_rows<-which(Reduce(`|`, lapply(resp_names, startsWith, x = as.character(all_codes$full_name))))

resp_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(resp_codes, match, x = as.character(EVENTS$event_code))))
resp_EV_ID<-(EVENTS$person_id[my_rows])
resp_EV_Date<- (EVENTS$start_date_record[my_rows])


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
my_rows<-which(Reduce(`|`, lapply(liver_names, startsWith, x = as.character(all_codes$full_name))))

liver_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(liver_codes, match, x = as.character(EVENTS$event_code))))
liver_EV_ID<-(EVENTS$person_id[my_rows])
liver_EV_Date<- (EVENTS$start_date_record[my_rows])

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
my_rows<-which(Reduce(`|`, lapply(HIV_names, startsWith, x = as.character(all_codes$full_name))))

HIV_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(HIV_codes, match, x = as.character(EVENTS$event_code))))
HIV_EV_ID<-(EVENTS$person_id[my_rows])
HIV_EV_Date<- (EVENTS$start_date_record[my_rows])


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
my_rows<-which(Reduce(`|`, lapply(kidney_names, startsWith, x = as.character(all_codes$full_name))))

kidney_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(kidney_codes, match, x = as.character(EVENTS$event_code))))
kidney_EV_ID<-(EVENTS$person_id[my_rows])
kidney_EV_Date<- (EVENTS$start_date_record[my_rows])


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
my_rows<-which(Reduce(`|`, lapply(diabetes_names, startsWith, x = as.character(all_codes$full_name))))

diabetes_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(diabetes_codes, match, x = as.character(EVENTS$event_code))))
diabetes_EV_ID<-(EVENTS$person_id[my_rows])
diabetes_EV_Date<- (EVENTS$start_date_record[my_rows])

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
my_rows<-which(Reduce(`|`, lapply(obesity_names, startsWith, x = as.character(all_codes$full_name))))

obesity_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(obesity_codes, match, x = as.character(EVENTS$event_code))))
obesity_EV_ID<-(EVENTS$person_id[my_rows])
obesity_EV_Date<- (EVENTS$start_date_record[my_rows])

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
my_rows<-which(Reduce(`|`, lapply(sickle_names, startsWith, x = as.character(all_codes$full_name))))

sickle_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(sickle_codes, match, x = as.character(EVENTS$event_code))))
sickle_EV_ID<-(EVENTS$person_id[my_rows])
sickle_EV_Date<- (EVENTS$start_date_record[my_rows])

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


immsup_names<-c("Im_TRANSPLANTRECIPIENT_COV")
my_rows<-which(Reduce(`|`, lapply(immsup_names, startsWith, x = as.character(all_codes$full_name))))

immsup_codes<- unique(c(all_codes$code[my_rows], all_codes$code_no_dots[my_rows]))

my_rows<-which(Reduce(`|`, lapply(immsup_codes, match, x = as.character(EVENTS$event_code))))
immsup_EV_ID<-(EVENTS$person_id[my_rows])
immsup_EV_Date<- (EVENTS$start_date_record[my_rows])

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
