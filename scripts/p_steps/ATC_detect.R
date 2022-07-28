#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#8/4/2022
#update 13/6/2022

#CONSIGN

#identify use of medicines over time for each cohort

#pregnancy_filter--> CDM only of pregnant women

my_path<-preselect_folder

#pattern match to find and loop through all the MEDS tables


my_dt_MED<-IMPORT_PATTERN(pat="MEDICINES", dir = my_path)


df <- select(my_dt_MED, date_dispensing, date_prescription)


drug_date<-df %>% transmute(Label = coalesce(date_dispensing, date_prescription))

my_dt_MED$drug_date<-drug_date

#####################################################################

Antihypertensives<-c("C02", "C03", "C04", "C07", "C08", "C09")

  my_rows<-which(Reduce(`|`, lapply(Antihypertensives, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
 Antihypertensives_ID<-(my_dt_MED$person_id[my_rows])
 Antihypertensives_Date<- (my_dt_MED$drug_date[my_rows])
 Antihypertensives_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])




Antihypertensives_df<-as.data.frame(cbind(Antihypertensives_ID,Antihypertensives_Date, Antihypertensives_ATC))
colnames(Antihypertensives_df)<-c("person_id", "date", "ATC")

fwrite(Antihypertensives_df, paste0(output_drugs, "Antihypertensives.csv"))

##########################################################################################

Antithrombotic<- ("B01") 


  my_rows<-which(Reduce(`|`, lapply(Antithrombotic, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antithrombotic_ID<-(my_dt_MED$person_id[my_rows])
  Antithrombotic_Date<- (my_dt_MED$drug_date[my_rows])
  Antithrombotic_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Antithrombotic_df<-as.data.frame(cbind(Antithrombotic_ID,Antithrombotic_Date, Antithrombotic_ATC))
colnames(Antithrombotic_df)<-c("person_id", "date", "ATC")

fwrite(Antithrombotic_df, paste0(output_drugs, "Antithrombotic.csv"))

##########################################################################################

Antivirals<-("J05") 


  my_rows<-which(Reduce(`|`, lapply(Antivirals, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antivirals_ID<-(my_dt_MED$person_id[my_rows])
  Antivirals_Date<- (my_dt_MED$drug_date[my_rows])
  Antivirals_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])




Antivirals_df<-as.data.frame(cbind(Antivirals_ID,Antivirals_Date, Antivirals_ATC))
colnames(Antivirals_df)<-c("person_id", "date", "ATC")

fwrite(Antivirals_df, paste0(output_drugs, "Antivirals.csv"))

##########################################################################################

Antibacterials<-("J01")
 

  my_rows<-which(Reduce(`|`, lapply(Antibacterials, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antibacterials_ID<-(my_dt_MED$person_id[my_rows])
  Antibacterials_Date<- (my_dt_MED$drug_date[my_rows])
  Antibacterials_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])




Antibacterials_df<-as.data.frame(cbind(Antibacterials_ID,Antibacterials_Date, Antibacterials_ATC))
colnames(Antibacterials_df)<-c("person_id", "date", "ATC")

fwrite(Antibacterials_df, paste0(output_drugs, "Antibacterials.csv"))


##########################################################################################

Antimycotics<-("J02")


  my_rows<-which(Reduce(`|`, lapply(Antimycotics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antimycotics_ID<-(my_dt_MED$person_id[my_rows])
  Antimycotics_Date<- (my_dt_MED$drug_date[my_rows])
  Antimycotics_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Antimycotics_df<-as.data.frame(cbind(Antimycotics_ID,Antimycotics_Date, Antimycotics_ATC))
colnames(Antimycotics_df)<-c("person_id", "date", "ATC")

fwrite(Antimycotics_df, paste0(output_drugs, "Antimycotics.csv"))

##########################################################################################

Antimycobacterials<-("J04") 

  my_rows<-which(Reduce(`|`, lapply(Antimycobacterials, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antimycobacterials_ID<-(my_dt_MED$person_id[my_rows])
  Antimycobacterials_Date<- (my_dt_MED$drug_date[my_rows])
  Antimycobacterials_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])




Antimycobacterials_df<-as.data.frame(cbind(Antimycobacterials_ID,Antimycobacterials_Date, Antimycobacterials_ATC))
colnames(Antimycobacterials_df)<-c("person_id", "date", "ATC")

fwrite(Antimycobacterials_df, paste0(output_drugs, "Antimycobacterials.csv"))



##########################################################################################

Immune_sera_globulins<-("J06") 


  my_rows<-which(Reduce(`|`, lapply(Immune_sera_globulins, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Immune_sera_globulins_ID<-(my_dt_MED$person_id[my_rows])
  Immune_sera_globulins_Date<- (my_dt_MED$drug_date[my_rows])
  Immune_sera_globulins_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Immune_sera_globulins_df<-as.data.frame(cbind(Immune_sera_globulins_ID,Immune_sera_globulins_Date, Immune_sera_globulins_ATC))
colnames(Immune_sera_globulins_df)<-c("person_id", "date", "ATC")

fwrite(Immune_sera_globulins_df, paste0(output_drugs, "Immune_sera_globulins.csv"))


##########################################################################################

Vaccinations<- ("J07") 

  my_rows<-which(Reduce(`|`, lapply(Vaccinations, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Vaccinations_ID<-(my_dt_MED$person_id[my_rows])
  Vaccinations_Date<- (my_dt_MED$drug_date[my_rows])
  Vaccinations_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])


Vaccinations_df<-as.data.frame(cbind(Vaccinations_ID,Vaccinations_Date, Vaccinations_ATC))
colnames(Vaccinations_df)<-c("person_id", "date", "ATC")


fwrite(Vaccinations_df, paste0(output_drugs, "Vaccinations.csv"))

##########################################################################################

Analgesics <-("N02")


  my_rows<-which(Reduce(`|`, lapply(Analgesics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Analgesics_ID<-(my_dt_MED$person_id[my_rows])
  Analgesics_Date<- (my_dt_MED$drug_date[my_rows])
  Analgesics_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])


Analgesics_df<-as.data.frame(cbind(Analgesics_ID,Analgesics_Date, Analgesics_ATC))
colnames(Analgesics_df)<-c("person_id", "date", "ATC")

fwrite(Analgesics_df, paste0(output_drugs, "Analgesics.csv"))


##########################################################################################

Psycholeptics <-("N05") 


  my_rows<-which(Reduce(`|`, lapply(Psycholeptics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Psycholeptics_ID<-(my_dt_MED$person_id[my_rows])
  Psycholeptics_Date<- (my_dt_MED$drug_date[my_rows])
  Psycholeptics_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])


Psycholeptics_df<-as.data.frame(cbind(Psycholeptics_ID,Psycholeptics_Date, Psycholeptics_ATC))
colnames(Psycholeptics_df)<-c("person_id", "date", "ATC")

fwrite(Psycholeptics_df, paste0(output_drugs, "Psycholeptics.csv"))

 
##########################################################################################
Psychoanaleptics<- ("N06") 


  my_rows<-which(Reduce(`|`, lapply(Psychoanaleptics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Psychoanaleptics_ID<-(my_dt_MED$person_id[my_rows])
  Psychoanaleptics_Date<- (my_dt_MED$drug_date[my_rows])
  Psychoanaleptics_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Psychoanaleptics_df<-as.data.frame(cbind(Psychoanaleptics_ID,Psychoanaleptics_Date, Psychoanaleptics_ATC))
colnames(Psychoanaleptics_df)<-c("person_id", "date", "ATC")
fwrite(Psychoanaleptics_df, paste0(output_drugs, "Psychoanaleptics.csv"))

##########################################################################################

Diabetes<- ("A10")

  
  my_rows<-which(Reduce(`|`, lapply(Diabetes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Diabetes_ID<-(my_dt_MED$person_id[my_rows])
  Diabetes_Date<- (my_dt_MED$drug_date[my_rows])
  Diabetes_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])


Diabetes_df<-as.data.frame(cbind(Diabetes_ID,Diabetes_Date, Diabetes_ATC))
colnames(Diabetes_df)<-c("person_id", "date", "ATC")
fwrite(Diabetes_df, paste0(output_drugs, "Diabetes.csv"))


##########################################################################################

Corticosteroids<- ("H02") 


  my_rows<-which(Reduce(`|`, lapply(Corticosteroids, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Corticosteroids_ID<-(my_dt_MED$person_id[my_rows])
  Corticosteroids_Date<- (my_dt_MED$drug_date[my_rows])
  Corticosteroids_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Corticosteroids_df<-as.data.frame(cbind(Corticosteroids_ID,Corticosteroids_Date, Corticosteroids_ATC))
colnames(Corticosteroids_df)<-c("person_id", "date", "ATC")

fwrite(Corticosteroids_df, paste0(output_drugs, "Corticosteroids.csv"))


##########################################################################################

Immunostimulants<- ("L03") 


  my_rows<-which(Reduce(`|`, lapply(Immunostimulants, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Immunostimulants_ID<-(my_dt_MED$person_id[my_rows])
  Immunostimulants_Date<- (my_dt_MED$drug_date[my_rows])
  Immunostimulants_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Immunostimulants_df<-as.data.frame(cbind(Immunostimulants_ID,Immunostimulants_Date, Immunostimulants_ATC))
colnames(Immunostimulants_df)<-c("person_id", "date", "ATC")

fwrite(Immunostimulants_df, paste0(output_drugs, "Immunostimulants.csv"))


##########################################################################################

Immunosuppressants<- ("L04")


  my_rows<-which(Reduce(`|`, lapply(Immunosuppressants, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Immunosuppressants_ID<-(my_dt_MED$person_id[my_rows])
  Immunosuppressants_Date<- (my_dt_MED$drug_date[my_rows])
  Immunosuppressants_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Immunosuppressants_df<-as.data.frame(cbind(Immunosuppressants_ID,Immunosuppressants_Date, Immunosuppressants_ATC))
colnames(Immunosuppressants_df)<-c("person_id", "date", "ATC")

fwrite(Immunosuppressants_df, paste0(output_drugs, "Immunosuppressants.csv"))


##########################################################################################

Anti_inflammatory<- ("M01") 


  my_rows<-which(Reduce(`|`, lapply(Anti_inflammatory, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Anti_inflammatory_ID<-(my_dt_MED$person_id[my_rows])
  Anti_inflammatory_Date<- (my_dt_MED$drug_date[my_rows])
  Anti_inflammatory_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Anti_inflammatory_df<-as.data.frame(cbind(Anti_inflammatory_ID,Anti_inflammatory_Date, Anti_inflammatory_ATC))
colnames(Anti_inflammatory_df)<-c("person_id", "date", "ATC")

fwrite(Anti_inflammatory_df, paste0(output_drugs, "Anti_inflammatory.csv"))


##########################################################################################

Nasal <- ("R01")  



  my_rows<-which(Reduce(`|`, lapply(Nasal, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Nasal_ID<-(my_dt_MED$person_id[my_rows])
  Nasal_Date<- (my_dt_MED$drug_date[my_rows])
  Nasal_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Nasal_df<-as.data.frame(cbind(Nasal_ID,Nasal_Date, Nasal_ATC))
colnames(Nasal_df)<-c("person_id", "date", "ATC")

fwrite(Nasal_df, paste0(output_drugs, "Nasal.csv"))

##########################################################################################

obstructive_airway<-("R03") 


my_rows<-which(Reduce(`|`, lapply(obstructive_airway, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
obstructive_airway_ID<-(my_dt_MED$person_id[my_rows])
obstructive_airway_Date<- (my_dt_MED$drug_date[my_rows])
obstructive_airway_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])


obstructive_airway_df<-as.data.frame(cbind(obstructive_airway_ID,obstructive_airway_Date, obstructive_airway_ATC))
colnames(obstructive_airway_df)<-c("person_id", "date", "ATC")

fwrite(obstructive_airway_df, paste0(output_drugs, "obstructive_airway.csv"))


##########################################################################################

Cough_cold <- ("R05")


  my_rows<-which(Reduce(`|`, lapply(Cough_cold, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Cough_cold_ID<-(my_dt_MED$person_id[my_rows])
  Cough_cold_Date<- (my_dt_MED$drug_date[my_rows])
  Cough_cold_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])



Cough_cold_df<-as.data.frame(cbind(Cough_cold_ID,Cough_cold_Date, Cough_cold_ATC))
colnames(Cough_cold_df)<-c("person_id", "date", "ATC")

fwrite(Cough_cold_df, paste0(output_drugs, "Cough_cold.csv"))



