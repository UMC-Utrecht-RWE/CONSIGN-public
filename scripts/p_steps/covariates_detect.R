#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#4/8/2022

# CONSIGN
# this script will identify the target codes for each covariate
# covariates described in Teams file CONSIGN_Variables.xlsx using version 9/15/22

cohort_folders<-list(cov_neg_pan_preg_folder, cov_pos_pan_preg_folder, cov_pos_not_preg_folder)
output_folders<-list(output_folder_neg_pan_preg, output_folder_pos_pan_preg, output_folder_pos_non_preg)


# only events within 1 year before covid+ pregnancy start date
# filter source data events everything before Jan 1 2019 (too old to be within covid preg window)

for(i in 1:length(cohort_folders)){
  
cohort_folder<-unlist(cohort_folders[i])

output_folder<-unlist(output_folders[i])

EVENTS<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir=cohort_folder)
MED_OB<-IMPORT_PATTERN(pat="MED_OB_SLIM", dir=cohort_folder)
SURV_OB<-IMPORT_PATTERN(pat="SURVEY_SLIM", dir=cohort_folder)
MED<-IMPORT_PATTERN(pat="MEDICINES_SLIM", dir=cohort_folder)
df <- select(MED, date_dispensing, date_prescription)
drug_date<-df %>% transmute(Label = coalesce(date_dispensing, date_prescription))
MED$drug_date<-as.numeric(drug_date)

all_codes<-fread(paste0(projectFolder,"/ALL_full_codelist.csv"))

#################################################################
#CARDIO



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
CARDIO_MED_Date<- (MED$drug_date[my_rows])

cardio_id<-c(CARDIO_EV_ID, CARDIO_MO_ID, CARDIO_SO_ID, CARDIO_MED_ID)
cardio_date<-c(CARDIO_EV_Date, CARDIO_MO_Date, CARDIO_SO_Date, CARDIO_MED_Date)
cardio_cov<-as.data.frame(cbind(cardio_id, cardio_date))

fwrite(cardio_cov, paste0(output_folder,"cardio.csv"))

#################################################################################
# CANCER

cancer_names<-c("Onc_ANYMALIGNANCY_COV")
my_rows<-which(Reduce(`|`, lapply(cancer_names, startsWith, x = as.character(all_codes$full_name))))

cancer_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(cancer_codes, startsWith, x = as.character(EVENTS$event_code))))
cancer_EV_ID<-(EVENTS$person_id[my_rows])
cancer_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(cancer_codes, startsWith, x = as.character(MED_OB$mo_code))))
cancer_MO_ID<-(MED_OB$person_id[my_rows])
cancer_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(cancer_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
cancer_SO_ID<-(SURV_OB$person_id[my_rows])
cancer_SO_Date<- (SURV_OB$so_date[my_rows])

cancer_atc<-c("L01A","L01B","L01C","L01D","L01X","L02A","L02B","L03","L04")

my_rows<-which(Reduce(`|`, lapply(cancer_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
cancer_MED_ID<-(MED$person_id[my_rows])
cancer_MED_Date<- (MED$drug_date[my_rows])

cancer_id<-c(cancer_EV_ID, cancer_MO_ID, cancer_SO_ID, cancer_MED_ID)
cancer_date<-c(cancer_EV_Date, cancer_MO_Date, cancer_SO_Date, cancer_MED_Date)
cancer_cov<-as.data.frame(cbind(cancer_id, cancer_date))

fwrite(cancer_cov, paste0(output_folder,"cancer.csv"))

#################################################################################
# RESPIRATORY


resp_names<-c("R_RESPCHRONIC_COV")
my_rows<-which(Reduce(`|`, lapply(resp_names, startsWith, x = as.character(all_codes$full_name))))

resp_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(resp_codes, startsWith, x = as.character(EVENTS$event_code))))
resp_EV_ID<-(EVENTS$person_id[my_rows])
resp_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(resp_codes, startsWith, x = as.character(MED_OB$mo_code))))
resp_MO_ID<-(MED_OB$person_id[my_rows])
resp_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(resp_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
resp_SO_ID<-(SURV_OB$person_id[my_rows])
resp_SO_Date<- (SURV_OB$so_date[my_rows])

resp_atc<-c("R03","R07AA","R07AB")

my_rows<-which(Reduce(`|`, lapply(resp_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
resp_MED_ID<-(MED$person_id[my_rows])
resp_MED_Date<- (MED$drug_date[my_rows])

resp_id<-c(resp_EV_ID, resp_MO_ID, resp_SO_ID, resp_MED_ID)
resp_date<-c(resp_EV_Date, resp_MO_Date, resp_SO_Date, resp_MED_Date)
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

liver_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(liver_codes, startsWith, x = as.character(EVENTS$event_code))))
liver_EV_ID<-(EVENTS$person_id[my_rows])
liver_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(liver_codes, startsWith, x = as.character(MED_OB$mo_code))))
liver_MO_ID<-(MED_OB$person_id[my_rows])
liver_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(liver_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
liver_SO_ID<-(SURV_OB$person_id[my_rows])
liver_SO_Date<- (SURV_OB$so_date[my_rows])

liver_atc<-c("J05AP")

my_rows<-which(Reduce(`|`, lapply(liver_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
liver_MED_ID<-(MED$person_id[my_rows])
liver_MED_Date<- (MED$drug_date[my_rows])

liver_id<-c(liver_EV_ID, liver_MO_ID, liver_SO_ID, liver_MED_ID)
liver_date<-c(liver_EV_Date, liver_MO_Date, liver_SO_Date, liver_MED_Date)
liver_cov<-as.data.frame(cbind(liver_id, liver_date))

fwrite(liver_cov, paste0(output_folder,"liver.csv"))
#################################################################################
# HIV


HIV_names<-c("I_HIVNOAIDS_CH")
my_rows<-which(Reduce(`|`, lapply(HIV_names, startsWith, x = as.character(all_codes$full_name))))

HIV_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(HIV_codes, startsWith, x = as.character(EVENTS$event_code))))
HIV_EV_ID<-(EVENTS$person_id[my_rows])
HIV_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(HIV_codes, startsWith, x = as.character(MED_OB$mo_code))))
HIV_MO_ID<-(MED_OB$person_id[my_rows])
HIV_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(HIV_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
HIV_SO_ID<-(SURV_OB$person_id[my_rows])
HIV_SO_Date<- (SURV_OB$so_date[my_rows])

HIV_atc<-c("J05AE","J05AR", "J05AF","J05AG")

my_rows<-which(Reduce(`|`, lapply(HIV_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
HIV_MED_ID<-(MED$person_id[my_rows])
HIV_MED_Date<- (MED$drug_date[my_rows])

HIV_id<-c(HIV_EV_ID, HIV_MO_ID, HIV_SO_ID, HIV_MED_ID)
HIV_date<-c(HIV_EV_Date, HIV_MO_Date, HIV_SO_Date, HIV_MED_Date)
HIV_cov<-as.data.frame(cbind(HIV_id, HIV_date))

fwrite(HIV_cov, paste0(output_folder,"HIV.csv"))

#################################################################################
# KIDNEY


kidney_names<-c("G_KDCHRONIC_COV")
my_rows<-which(Reduce(`|`, lapply(kidney_names, startsWith, x = as.character(all_codes$full_name))))

kidney_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(kidney_codes, startsWith, x = as.character(EVENTS$event_code))))
kidney_EV_ID<-(EVENTS$person_id[my_rows])
kidney_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(kidney_codes, startsWith, x = as.character(MED_OB$mo_code))))
kidney_MO_ID<-(MED_OB$person_id[my_rows])
kidney_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(kidney_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
kidney_SO_ID<-(SURV_OB$person_id[my_rows])
kidney_SO_Date<- (SURV_OB$so_date[my_rows])

kidney_atc<-c("B03XA01")

my_rows<-which(Reduce(`|`, lapply(kidney_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
kidney_MED_ID<-(MED$person_id[my_rows])
kidney_MED_Date<- (MED$drug_date[my_rows])

kidney_id<-c(kidney_EV_ID, kidney_MO_ID, kidney_SO_ID, kidney_MED_ID)
kidney_date<-c(kidney_EV_Date, kidney_MO_Date, kidney_SO_Date, kidney_MED_Date)
kidney_cov<-as.data.frame(cbind(kidney_id, kidney_date))

fwrite(kidney_cov, paste0(output_folder,"kidney.csv"))

#################################################################################
# DIABETES


diabetes_names<-c("E_DM12_COV")
my_rows<-which(Reduce(`|`, lapply(diabetes_names, startsWith, x = as.character(all_codes$full_name))))

diabetes_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(diabetes_codes, startsWith, x = as.character(EVENTS$event_code))))
diabetes_EV_ID<-(EVENTS$person_id[my_rows])
diabetes_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(diabetes_codes, startsWith, x = as.character(MED_OB$mo_code))))
diabetes_MO_ID<-(MED_OB$person_id[my_rows])
diabetes_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(diabetes_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
diabetes_SO_ID<-(SURV_OB$person_id[my_rows])
diabetes_SO_Date<- (SURV_OB$so_date[my_rows])

diabetes_atc<-c("A10B","A10A")

my_rows<-which(Reduce(`|`, lapply(diabetes_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
diabetes_MED_ID<-(MED$person_id[my_rows])
diabetes_MED_Date<- (MED$drug_date[my_rows])

diabetes_id<-c(diabetes_EV_ID, diabetes_MO_ID, diabetes_SO_ID, diabetes_MED_ID)
diabetes_date<-c(diabetes_EV_Date, diabetes_MO_Date, diabetes_SO_Date, diabetes_MED_Date)
diabetes_cov<-as.data.frame(cbind(diabetes_id, diabetes_date))

fwrite(diabetes_cov, paste0(output_folder,"diabetes.csv"))

#################################################################################
# OBESITY


obesity_names<-c("L_OBESITY_COV")
my_rows<-which(Reduce(`|`, lapply(obesity_names, startsWith, x = as.character(all_codes$full_name))))

obesity_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(obesity_codes, startsWith, x = as.character(EVENTS$event_code))))
obesity_EV_ID<-(EVENTS$person_id[my_rows])
obesity_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(obesity_codes, startsWith, x = as.character(MED_OB$mo_code))))
obesity_MO_ID<-(MED_OB$person_id[my_rows])
obesity_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(obesity_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
obesity_SO_ID<-(SURV_OB$person_id[my_rows])
obesity_SO_Date<- (SURV_OB$so_date[my_rows])

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

sickle_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(sickle_codes, startsWith, x = as.character(EVENTS$event_code))))
sickle_EV_ID<-(EVENTS$person_id[my_rows])
sickle_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(sickle_codes, startsWith, x = as.character(MED_OB$mo_code))))
sickle_MO_ID<-(MED_OB$person_id[my_rows])
sickle_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(sickle_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
sickle_SO_ID<-(SURV_OB$person_id[my_rows])
sickle_SO_Date<- (SURV_OB$so_date[my_rows])

sickle_atc<-c("L01XX05","B06AX")

my_rows<-which(Reduce(`|`, lapply(sickle_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
sickle_MED_ID<-(MED$person_id[my_rows])
sickle_MED_Date<- (MED$drug_date[my_rows])

sickle_id<-c(sickle_EV_ID, sickle_MO_ID, sickle_SO_ID, sickle_MED_ID)
sickle_date<-c(sickle_EV_Date, sickle_MO_Date, sickle_SO_Date, sickle_MED_Date)
sickle_cov<-as.data.frame(cbind(sickle_id, sickle_date))

fwrite(sickle_cov, paste0(output_folder,"sickle.csv"))

#################################################################################
# IMMUNOSUPRESSION


immsup_names<-c("Im_TRANSPLANTRECIPIENT_COV")
my_rows<-which(Reduce(`|`, lapply(immsup_names, startsWith, x = as.character(all_codes$full_name))))

immsup_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(immsup_codes, startsWith, x = as.character(EVENTS$event_code))))
immsup_EV_ID<-(EVENTS$person_id[my_rows])
immsup_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(immsup_codes, startsWith, x = as.character(MED_OB$mo_code))))
immsup_MO_ID<-(MED_OB$person_id[my_rows])
immsup_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(immsup_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
immsup_SO_ID<-(SURV_OB$person_id[my_rows])
immsup_SO_Date<- (SURV_OB$so_date[my_rows])

immsup_atc<-c("L04A","H02")

my_rows<-which(Reduce(`|`, lapply(immsup_atc, startsWith, x = as.character(MED$medicinal_product_atc_code))))
immsup_MED_ID<-(MED$person_id[my_rows])
immsup_MED_Date<- (MED$drug_date[my_rows])

immsup_id<-c(immsup_EV_ID, immsup_MO_ID, immsup_SO_ID, immsup_MED_ID)
immsup_date<-c(immsup_EV_Date, immsup_MO_Date, immsup_SO_Date, immsup_MED_Date)
immsup_cov<-as.data.frame(cbind(immsup_id, immsup_date))

fwrite(immsup_cov, paste0(output_folder,"immunosupression.csv"))

#################################################################################
# MENTAL


mental_names<-c("DP_COVMENTALHEALTH")
my_rows<-which(Reduce(`|`, lapply(mental_names, startsWith, x = as.character(all_codes$full_name))))

mental_codes<- unique(all_codes$code[my_rows])

my_rows<-which(Reduce(`|`, lapply(mental_codes, startsWith, x = as.character(EVENTS$event_code))))
mental_EV_ID<-(EVENTS$person_id[my_rows])
mental_EV_Date<- (EVENTS$start_date_record[my_rows])

my_rows<-which(Reduce(`|`, lapply(mental_codes, startsWith, x = as.character(MED_OB$mo_code))))
mental_MO_ID<-(MED_OB$person_id[my_rows])
mental_MO_Date<- (MED_OB$mo_date[my_rows])

my_rows<-which(Reduce(`|`, lapply(mental_codes, startsWith, x = as.character(SURV_OB$so_meaning))))
mental_SO_ID<-(SURV_OB$person_id[my_rows])
mental_SO_Date<- (SURV_OB$so_date[my_rows])

mental_id<-c(mental_EV_ID, mental_MO_ID, mental_SO_ID)
mental_date<-c(mental_EV_Date, mental_MO_Date, mental_SO_Date)
mental_cov<-as.data.frame(cbind(mental_id, mental_date))

fwrite(mental_cov, paste0(output_folder,"mental.csv"))

}
