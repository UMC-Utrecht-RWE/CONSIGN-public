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


my_MED_tables<-list.files(path=my_path, pattern = "MEDICINES_")


Antihypertensives<-c("C02", "C03", "C04", "C07", "C08", "C09")
Antihypertensives_ID<-list()
Antihypertensives_Date<-list()
Antihypertensives_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Antihypertensives, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
 Antihypertensives_ID[j]<-list(my_dt_MED$person_id[my_rows])
 Antihypertensives_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
 Antihypertensives_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Antihypertensives_ID<-unlist(Antihypertensives_ID)
Antihypertensives_Date<-unlist(Antihypertensives_Date)
Antihypertensives_ATC<-unlist(Antihypertensives_ATC)
Antihypertensives_df<-as.data.frame(cbind(Antihypertensives_ID,Antihypertensives_Date, Antihypertensives_ATC))
colnames(Antihypertensives_df)<-c("person_id", "date", "ATC")

fwrite(Antihypertensives_df, paste0(output_drugs, "Antihypertensives.csv"))

##########################################################################################

Antithrombotic<- ("B01") 

Antithrombotic_ID<-list()
Antithrombotic_Date<-list()
Antithrombotic_ATC<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Antithrombotic, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antithrombotic_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Antithrombotic_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Antithrombotic_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}

Antithrombotic_ID<-unlist(Antithrombotic_ID)
Antithrombotic_Date<-unlist(Antithrombotic_Date)
Antithrombotic_ATC<-unlist(Antithrombotic_ATC)
Antithrombotic_df<-as.data.frame(cbind(Antithrombotic_ID,Antithrombotic_Date, Antithrombotic_ATC))
colnames(Antithrombotic_df)<-c("person_id", "date", "ATC")

fwrite(Antithrombotic_df, paste0(output_drugs, "Antithrombotic.csv"))

##########################################################################################

Antivirals<-("J05") 

Antivirals_ID<-list()
Antivirals_Date<-list()
Antivirals_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Antivirals, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antivirals_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Antivirals_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Antivirals_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Antivirals_ID<-unlist(Antivirals_ID)
Antivirals_Date<-unlist(Antivirals_Date)
Antivirals_ATC<-unlist(Antivirals_ATC)
Antivirals_df<-as.data.frame(cbind(Antivirals_ID,Antivirals_Date, Antivirals_ATC))
colnames(Antivirals_df)<-c("person_id", "date", "ATC")

fwrite(Antivirals_df, paste0(output_drugs, "Antivirals.csv"))

##########################################################################################

Antibacterials<-("J01")
 

Antibacterials_ID<-list()
Antibacterials_Date<-list()
Antibacterials_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Antibacterials, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antibacterials_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Antibacterials_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Antibacterials_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Antibacterials_ID<-unlist(Antibacterials_ID)
Antibacterials_Date<-unlist(Antibacterials_Date)
Antibacterials_ATC<-unlist(Antibacterials_ATC)
Antibacterials_df<-as.data.frame(cbind(Antibacterials_ID,Antibacterials_Date, Antibacterials_ATC))
colnames(Antibacterials_df)<-c("person_id", "date", "ATC")

fwrite(Antibacterials_df, paste0(output_drugs, "Antibacterials.csv"))


##########################################################################################

Antimycotics<-("J02")

Antimycotics_ID<-list()
Antimycotics_Date<-list()
Antimycotics_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Antimycotics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antimycotics_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Antimycotics_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Antimycotics_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Antimycotics_ID<-unlist(Antimycotics_ID)
Antimycotics_Date<-unlist(Antimycotics_Date)
Antimycotics_ATC<-unlist(Antimycotics_ATC)
Antimycotics_df<-as.data.frame(cbind(Antimycotics_ID,Antimycotics_Date, Antimycotics_ATC))
colnames(Antimycotics_df)<-c("person_id", "date", "ATC")

fwrite(Antimycotics_df, paste0(output_drugs, "Antimycotics.csv"))

##########################################################################################

Antimycobacterials<-("J04") 

Antimycobacterials_ID<-list()
Antimycobacterials_Date<-list()
Antimycobacterials_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Antimycobacterials, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Antimycobacterials_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Antimycobacterials_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Antimycobacterials_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Antimycobacterials_ID<-unlist(Antimycobacterials_ID)
Antimycobacterials_Date<-unlist(Antimycobacterials_Date)
Antimycobacterials_ATC<-unlist(Antimycobacterials_ATC)
Antimycobacterials_df<-as.data.frame(cbind(Antimycobacterials_ID,Antimycobacterials_Date, Antimycobacterials_ATC))
colnames(Antimycobacterials_df)<-c("person_id", "date", "ATC")

fwrite(Antimycobacterials_df, paste0(output_drugs, "Antimycobacterials.csv"))



##########################################################################################

Immune_sera_globulins<-("J06") 

Immune_sera_globulins_ID<-list()
Immune_sera_globulins_Date<-list()
Immune_sera_globulins_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Immune_sera_globulins, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Immune_sera_globulins_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Immune_sera_globulins_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Immune_sera_globulins_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Immune_sera_globulins_ID<-unlist(Immune_sera_globulins_ID)
Immune_sera_globulins_Date<-unlist(Immune_sera_globulins_Date)
Immune_sera_globulins_ATC<-unlist(Immune_sera_globulins_ATC)
Immune_sera_globulins_df<-as.data.frame(cbind(Immune_sera_globulins_ID,Immune_sera_globulins_Date, Immune_sera_globulins_ATC))
colnames(Immune_sera_globulins_df)<-c("person_id", "date", "ATC")

fwrite(Immune_sera_globulins_df, paste0(output_drugs, "Immune_sera_globulins.csv"))


##########################################################################################

Vaccinations<- ("J07") 

Vaccinations_ID<-list()
Vaccinations_Date<-list()
Vaccinations_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Vaccinations, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Vaccinations_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Vaccinations_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Vaccinations_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Vaccinations_ID<-unlist(Vaccinations_ID)
Vaccinations_Date<-unlist(Vaccinations_Date)
Vaccinations_ATC<-unlist(Vaccinations_ATC)
Vaccinations_df<-as.data.frame(cbind(Vaccinations_ID,Vaccinations_Date, Vaccinations_ATC))
colnames(Vaccinations_df)<-c("person_id", "date", "ATC")


fwrite(Vaccinations_df, paste0(output_drugs, "Vaccinations.csv"))

##########################################################################################

Analgesics <-("N02")

Analgesics_ID<-list()
Analgesics_Date<-list()
Analgesics_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Analgesics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Analgesics_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Analgesics_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Analgesics_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Analgesics_ID<-unlist(Analgesics_ID)
Analgesics_Date<-unlist(Analgesics_Date)
Analgesics_ATC<-unlist(Analgesics_ATC)
Analgesics_df<-as.data.frame(cbind(Analgesics_ID,Analgesics_Date, Analgesics_ATC))
colnames(Analgesics_df)<-c("person_id", "date", "ATC")

fwrite(Analgesics_df, paste0(output_drugs, "Analgesics.csv"))


##########################################################################################

Psycholeptics <-("N05") 

Psycholeptics_ID<-list()
Psycholeptics_Date<-list()
Psycholeptics_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Psycholeptics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Psycholeptics_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Psycholeptics_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Psycholeptics_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Psycholeptics_ID<-unlist(Psycholeptics_ID)
Psycholeptics_Date<-unlist(Psycholeptics_Date)
Psycholeptics_ATC<-unlist(Psycholeptics_ATC)
Psycholeptics_df<-as.data.frame(cbind(Psycholeptics_ID,Psycholeptics_Date, Psycholeptics_ATC))
colnames(Psycholeptics_df)<-c("person_id", "date", "ATC")

fwrite(Psycholeptics_df, paste0(output_drugs, "Psycholeptics.csv"))

 
##########################################################################################
Psychoanaleptics<- ("N06") 

Psychoanaleptics_ID<-list()
Psychoanaleptics_Date<-list()
Psychoanaleptics_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Psychoanaleptics, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Psychoanaleptics_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Psychoanaleptics_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Psychoanaleptics_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Psychoanaleptics_ID<-unlist(Psychoanaleptics_ID)
Psychoanaleptics_Date<-unlist(Psychoanaleptics_Date)
Psychoanaleptics_ATC<-unlist(Psychoanaleptics_ATC)
Psychoanaleptics_df<-as.data.frame(cbind(Psychoanaleptics_ID,Psychoanaleptics_Date, Psychoanaleptics_ATC))
colnames(Psychoanaleptics_df)<-c("person_id", "date", "ATC")
fwrite(Psychoanaleptics_df, paste0(output_drugs, "Psychoanaleptics.csv"))

##########################################################################################

Diabetes<- ("A10")

Diabetes_ID<-list()
Diabetes_Date<-list()
Diabetes_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Diabetes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Diabetes_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Diabetes_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Diabetes_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Diabetes_ID<-unlist(Diabetes_ID)
Diabetes_Date<-unlist(Diabetes_Date)
Diabetes_ATC<-unlist(Diabetes_ATC)
Diabetes_df<-as.data.frame(cbind(Diabetes_ID,Diabetes_Date, Diabetes_ATC))
colnames(Diabetes_df)<-c("person_id", "date", "ATC")
fwrite(Diabetes_df, paste0(output_drugs, "Diabetes.csv"))


##########################################################################################

Corticosteroids<- ("H02") 

Corticosteroids_ID<-list()
Corticosteroids_Date<-list()
Corticosteroids_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Corticosteroids, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Corticosteroids_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Corticosteroids_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Corticosteroids_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Corticosteroids_ID<-unlist(Corticosteroids_ID)
Corticosteroids_Date<-unlist(Corticosteroids_Date)
Corticosteroids_ATC<-unlist(Corticosteroids_ATC)
Corticosteroids_df<-as.data.frame(cbind(Corticosteroids_ID,Corticosteroids_Date, Corticosteroids_ATC))
colnames(Corticosteroids_df)<-c("person_id", "date", "ATC")

fwrite(Corticosteroids_df, paste0(output_drugs, "Corticosteroids.csv"))


##########################################################################################

Immunostimulants<- ("L03") 

Immunostimulants_ID<-list()
Immunostimulants_Date<-list()
Immunostimulants_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Immunostimulants, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Immunostimulants_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Immunostimulants_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Immunostimulants_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Immunostimulants_ID<-unlist(Immunostimulants_ID)
Immunostimulants_Date<-unlist(Immunostimulants_Date)
Immunostimulants_ATC<-unlist(Immunostimulants_ATC)
Immunostimulants_df<-as.data.frame(cbind(Immunostimulants_ID,Immunostimulants_Date, Immunostimulants_ATC))
colnames(Immunostimulants_df)<-c("person_id", "date", "ATC")

fwrite(Immunostimulants_df, paste0(output_drugs, "Immunostimulants.csv"))


##########################################################################################

Immunosuppressants<- ("L04")

Immunosuppressants_ID<-list()
Immunosuppressants_Date<-list()
Immunosuppressants_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Immunosuppressants, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Immunosuppressants_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Immunosuppressants_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Immunosuppressants_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Immunosuppressants_ID<-unlist(Immunosuppressants_ID)
Immunosuppressants_Date<-unlist(Immunosuppressants_Date)
Immunosuppressants_ATC<-unlist(Immunosuppressants_ATC)
Immunosuppressants_df<-as.data.frame(cbind(Immunosuppressants_ID,Immunosuppressants_Date, Immunosuppressants_ATC))
colnames(Immunosuppressants_df)<-c("person_id", "date", "ATC")

fwrite(Immunosuppressants_df, paste0(output_drugs, "Immunosuppressants.csv"))


##########################################################################################

Anti_inflammatory<- ("M01") 

Anti_inflammatory_ID<-list()
Anti_inflammatory_Date<-list()
Anti_inflammatory_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Anti_inflammatory, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Anti_inflammatory_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Anti_inflammatory_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Anti_inflammatory_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Anti_inflammatory_ID<-unlist(Anti_inflammatory_ID)
Anti_inflammatory_Date<-unlist(Anti_inflammatory_Date)
Anti_inflammatory_ATC<-unlist(Anti_inflammatory_ATC)
Anti_inflammatory_df<-as.data.frame(cbind(Anti_inflammatory_ID,Anti_inflammatory_Date, Anti_inflammatory_ATC))
colnames(Anti_inflammatory_df)<-c("person_id", "date", "ATC")

fwrite(Anti_inflammatory_df, paste0(output_drugs, "Anti_inflammatory.csv"))


##########################################################################################

Nasal <- ("R01")  

Nasal_ID<-list()
Nasal_Date<-list()
Nasal_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Nasal, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Nasal_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Nasal_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Nasal_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Nasal_ID<-unlist(Nasal_ID)
Nasal_Date<-unlist(Nasal_Date)
Nasal_ATC<-unlist(Nasal_ATC)
Nasal_df<-as.data.frame(cbind(Nasal_ID,Nasal_Date, Nasal_ATC))
colnames(Nasal_df)<-c("person_id", "date", "ATC")

fwrite(Nasal_df, paste0(output_drugs, "Nasal.csv"))

obstructive_airway<-("R03") 

##########################################################################################

Cough_cold <- ("R05")

Cough_cold_ID<-list()
Cough_cold_Date<-list()
Cough_cold_ATC<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  my_rows<-which(Reduce(`|`, lapply(Cough_cold, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  Cough_cold_ID[j]<-list(my_dt_MED$person_id[my_rows])
  Cough_cold_Date[j]<- list(my_dt_MED$date_dispensing[my_rows])
  Cough_cold_ATC[j]<-list(my_dt_MED$medicinal_product_atc_code[my_rows])
}


Cough_cold_ID<-unlist(Cough_cold_ID)
Cough_cold_Date<-unlist(Cough_cold_Date)
Cough_cold_ATC<-unlist(Cough_cold_ATC)
Cough_cold_df<-as.data.frame(cbind(Cough_cold_ID,Cough_cold_Date, Cough_cold_ATC))
colnames(Cough_cold_df)<-c("person_id", "date", "ATC")

fwrite(Cough_cold_df, paste0(output_drugs, "Cough_cold.csv"))



