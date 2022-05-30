#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#8/4/2022

#CONSIGN

#identify use of medicines during pregnancy

#pregnancy_filter--> CDM only of pregnant women

my_path<-preselect_folder

#pattern match to find and loop through all the MEDS tables


my_MED_tables<-list.files(path=my_path, pattern = "MEDICINES_")


Antihypertensives<-c("C02", "C03", "C04", "C07", "C08", "C09")
Antihypertensives_ID<-list()
Antihypertensives_Date<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
 Antihypertensives_ID[j]<-list(my_dt_MED$person_id[Reduce(`|`, lapply(Antihypertensives, startsWith, x = as.character(my_dt_MED$MED_code)))])
 Antihypertensives_Date[j]<-list(my_dt_MED$start_date_record[Reduce(`|`, lapply(Antihypertensives, startsWith, x = as.character(my_dt_MED$MED_code)))])
}


Antihypertensives_ID<-unlist(Antihypertensives_ID)
Antihypertensives_Date<-unlist(Antihypertensives_Date)
Antihypertensives_df<-as.data.frame(cbind(Antihypertensives_ID,Antihypertensives_Date))
colnames(Antihypertensives_df)<-c("person_id", "date")

fwrite(Antihypertensives_df, paste0(output_drugs, "antihypertensives.csv"))

##########################################################################################

Antithrombotic<- ("B01") 

Antithrombotic_ID<-list()
Antithrombotic_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Antithrombotic_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Antithrombotic)]
  Antithrombotic_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Antithrombotic)]
}

Antithrombotic_df<-as.data.frame(cbind(unlist(Antithrombotic_ID), unlist(Antithrombotic_Date)))
colnames(Antithrombotic)<-c("person_id", "date")

fwrite(Antithrombotic_df, paste0(output_drugs, "Antithrombotic.csv"))

##########################################################################################

Antivirals<-("J05") 

Antivirals_ID<-list()
Antivirals_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Antivirals_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Antivirals)]
  Antivirals_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Antivirals)]
}

Antivirals_df<-as.data.frame(cbind(unlist(Antivirals_ID), unlist(Antivirals_Date)))
colnames(Antivirals)<-c("person_id", "date")

fwrite(Antivirals_df, paste0(output_drugs, "Antivirals.csv"))

##########################################################################################

Antibacterials<-("J01")
 

Antibacterials_ID<-list()
Antibacterials_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Antibacterials_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Antibacterials)]
  Antibacterials_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Antibacterials)]
}

Antibacterials_df<-as.data.frame(cbind(unlist(Antibacterials_ID), unlist(Antibacterials_Date)))
colnames(Antibacterials)<-c("person_id", "date")

fwrite(Antibacterials_df, paste0(output_drugs, "Antibacterials.csv"))

Antimycotics<-("J02")
##########################################################################################

Antimycotics<-("J02")

Antimycotics_ID<-list()
Antimycotics_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Antimycotics_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Antimycotics)]
  Antimycotics_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Antimycotics)]
}

Antimycotics_df<-as.data.frame(cbind(unlist(Antimycotics_ID), unlist(Antimycotics_Date)))
colnames(Antimycotics)<-c("person_id", "date")

fwrite(Antimycotics_df, paste0(output_drugs, "Antimycotics.csv"))

##########################################################################################

Antimycobacterials<-("J04") 

Antimycobacterials_ID<-list()
Antimycobacterials_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Antimycobacterials_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Antimycobacterials)]
  Antimycobacterials_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Antimycobacterials)]
}

Antimycobacterials_df<-as.data.frame(cbind(unlist(Antimycobacterials_ID), unlist(Antimycobacterials_Date)))
colnames(Antimycobacterials)<-c("person_id", "date")

fwrite(Antimycobacterials_df, paste0(output_drugs, "Antimycobacterials.csv"))



##########################################################################################

Immune_sera_globulins<-("J06") 

Immune_sera_globulins_ID<-list()
Immune_sera_globulins_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Immune_sera_globulins_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Immune_sera_globulins)]
  Immune_sera_globulins_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Immune_sera_globulins)]
}

Immune_sera_globulins_df<-as.data.frame(cbind(unlist(Immune_sera_globulins_ID), unlist(Immune_sera_globulins_Date)))
colnames(Immune_sera_globulins)<-c("person_id", "date")

fwrite(Immune_sera_globulins_df, paste0(output_drugs, "Immune_sera_globulins.csv"))


Vaccinations<- ("J07") 
##########################################################################################

Vaccinations<- ("J07") 

Vaccinations_ID<-list()
Vaccinations_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Vaccinations_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Vaccinations)]
  Vaccinations_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Vaccinations)]
}

Vaccinations_df<-as.data.frame(cbind(unlist(Vaccinations_ID), unlist(Vaccinations_Date)))
colnames(Vaccinations)<-c("person_id", "date")

fwrite(Vaccinations_df, paste0(output_drugs, "Vaccinations.csv"))


Analgesics <-("N02")
##########################################################################################

Analgesics <-("N02")

Analgesics_ID<-list()
Analgesics_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Analgesics_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Analgesics)]
  Analgesics_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Analgesics)]
}

Analgesics_df<-as.data.frame(cbind(unlist(Analgesics_ID), unlist(Analgesics_Date)))
colnames(Analgesics)<-c("person_id", "date")

fwrite(Analgesics_df, paste0(output_drugs, "Analgesics.csv"))


##########################################################################################

Psycholeptics <-("N05") 

Psycholeptics_ID<-list()
Psycholeptics_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Psycholeptics_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Psycholeptics)]
  Psycholeptics_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Psycholeptics)]
}

Psycholeptics_df<-as.data.frame(cbind(unlist(Psycholeptics_ID), unlist(Psycholeptics_Date)))
colnames(Psycholeptics)<-c("person_id", "date")

fwrite(Psycholeptics_df, paste0(output_drugs, "Psycholeptics.csv"))

 
##########################################################################################
Psychoanaleptics<- ("N06") 

Psychoanaleptics_ID<-list()
Psychoanaleptics_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Psychoanaleptics_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Psychoanaleptics)]
  Psychoanaleptics_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Psychoanaleptics)]
}

Psychoanaleptics_df<-as.data.frame(cbind(unlist(Psychoanaleptics_ID), unlist(Psychoanaleptics_Date)))
colnames(Psychoanaleptics)<-c("person_id", "date")

fwrite(Psychoanaleptics_df, paste0(output_drugs, "Psychoanaleptics.csv"))

Diabetes<- ("A10")
##########################################################################################

Diabetes<- ("A10")

Diabetes_ID<-list()
Diabetes_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Diabetes_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Diabetes)]
  Diabetes_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Diabetes)]
}

Diabetes_df<-as.data.frame(cbind(unlist(Diabetes_ID), unlist(Diabetes_Date)))
colnames(Diabetes)<-c("person_id", "date")

fwrite(Diabetes_df, paste0(output_drugs, "Diabetes.csv"))


##########################################################################################

Corticoisteroids<- ("H02") 

Corticoisteroids_ID<-list()
Corticoisteroids_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Corticoisteroids_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Corticoisteroids)]
  Corticoisteroids_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Corticoisteroids)]
}

Corticoisteroids_df<-as.data.frame(cbind(unlist(Corticoisteroids_ID), unlist(Corticoisteroids_Date)))
colnames(Corticoisteroids)<-c("person_id", "date")

fwrite(Corticoisteroids_df, paste0(output_drugs, "Corticoisteroids.csv"))


##########################################################################################

Immunostimulants<- ("L03") 

Immunostimulants_ID<-list()
Immunostimulants_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Immunostimulants_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Immunostimulants)]
  Immunostimulants_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Immunostimulants)]
}

Immunostimulants_df<-as.data.frame(cbind(unlist(Immunostimulants_ID), unlist(Immunostimulants_Date)))
colnames(Immunostimulants)<-c("person_id", "date")

fwrite(Immunostimulants_df, paste0(output_drugs, "Immunostimulants.csv"))


##########################################################################################

Immunosuppressants<- ("L04")

Immunosuppressants_ID<-list()
Immunosuppressants_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Immunosuppressants_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Immunosuppressants)]
  Immunosuppressants_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Immunosuppressants)]
}

Immunosuppressants_df<-as.data.frame(cbind(unlist(Immunosuppressants_ID), unlist(Immunosuppressants_Date)))
colnames(Immunosuppressants)<-c("person_id", "date")

fwrite(Immunosuppressants_df, paste0(output_drugs, "Immunosuppressants.csv"))


##########################################################################################

Anti_inflammatory<- ("M01") 

Anti_inflammatory_ID<-list()
Anti_inflammatory_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Anti_inflammatory_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Anti_inflammatory)]
  Anti_inflammatory_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Anti_inflammatory)]
}

Anti_inflammatory_df<-as.data.frame(cbind(unlist(Anti_inflammatory_ID), unlist(Anti_inflammatory_Date)))
colnames(Anti_inflammatory)<-c("person_id", "date")

fwrite(Anti_inflammatory_df, paste0(output_drugs, "Anti_inflammatory.csv"))


##########################################################################################

Nasal <- ("R01")  

Nasal_ID<-list()
Nasal_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Nasal_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Nasal)]
  Nasal_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Nasal)]
}

Nasal_df<-as.data.frame(cbind(unlist(Nasal_ID), unlist(Nasal_Date)))
colnames(Nasal)<-c("person_id", "date")

fwrite(Nasal_df, paste0(output_drugs, "Nasal.csv"))

obstructive_airway<-("R03") 
##########################################################################################

Antithrombotic<- ("B01") 

Antithrombotic_ID<-list()
Antithrombotic_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Antithrombotic_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Antithrombotic)]
  Antithrombotic_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Antithrombotic)]
}

Antithrombotic_df<-as.data.frame(cbind(unlist(Antithrombotic_ID), unlist(Antithrombotic_Date)))
colnames(Antithrombotic)<-c("person_id", "date")

fwrite(Antithrombotic_df, paste0(output_drugs, "Antithrombotic.csv"))


##########################################################################################

Cough_cold <- ("R05")

Cough_cold_ID<-list()
Cough_cold_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Cough_cold_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,Cough_cold)]
  Cough_cold_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,Cough_cold)]
}

Cough_cold_df<-as.data.frame(cbind(unlist(Cough_cold_ID), unlist(Cough_cold_Date)))
colnames(Cough_cold)<-c("person_id", "date")

fwrite(Cough_cold_df, paste0(output_drugs, "Cough_cold.csv"))



