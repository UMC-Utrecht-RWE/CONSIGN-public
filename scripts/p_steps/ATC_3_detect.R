#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#8/4/2022
#update 13/6/2022
#major revision 20/9/22 (made into function+machine readable input)

#CONSIGN

#identify use of medicines over time for women of reproduction age (later sorted into cohorts)

my_path<-preselect_folder

my_write_path<- raw_atc_3_counts

#pattern match to find and loop through all the MEDS tables


my_dt_MED<-IMPORT_PATTERN(pat="MEDICINES_SLIM", dir = my_path)


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

if(DAP=="USWAN"){
  my_dt_MED$drug_date<-my_dt_MED$date_prescription}else{my_dt_MED$drug_date<-my_dt_MED$date_dispensing}



# translate SAP ATC table into machine-readable format

names_drug_groups<-list( "Antithrombotic","Gluco_Corticoisteroids_systemic",
                        "AB_beta_lactam", "AB_beta_lactam_other",
                        "AB_Macrolides_Lincosamides_Streptogramins", "AB_Aminoglycoside",
                        "AB_other",  "Antivirals_Direct_Acting", 
                        "Immunoglobulins", "Vaccinations_Viral", "Antineoplastic_Kinase_inhibitors", 
                        "Immunostimulants", "Immunosuppressants", "Antigout", "Analgesics_Antipyretic_Other",
                         "Psychoanaleptics_Antidepressants", "Antiprotozoals_Amoebiasis_Other",
                        "Antiprotozoals_Malaria",
                        "Antinematodal", "Obstructive_Airway_Adrenergics")

ATC_groups<-list( "B01A" ,"H02A", "J01C", "J01D", "J01F","J01G","J01X",
                  "J05A" , "J06B", "J07B" ,  "L01E", "L03A", "L04A",  "M04A", "N02B",
                  "N06A" , "P01A","P01B", "P02C",   "R03A" )

# bind to data_frame to check that codes are aligned correctly
df_atc_groups<-as.data.frame(cbind((names_drug_groups), (ATC_groups)))

colnames(df_atc_groups)<-c("name", "ATC")

print(df_atc_groups)

fwrite(df_atc_groups, paste0(g_intermediate,"/ATC_3_source_data.csv"))
#####################################################################

ATC_detect<-function(my_data= my_dt_MED, names=names_drug_groups, ATC=ATC_groups){
  for(i in 1:length(names_drug_groups)){
  my_rows<-which(Reduce(`|`, lapply(ATC[i], startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code))))
  my_ID<-paste0(names[i],"_ID")
  my_ID<-(my_dt_MED$person_id[my_rows])
  my_Date<-paste0(names[i],"_Date")
  my_Date<- (my_dt_MED$drug_date[my_rows])
  my_ATC<-paste0(names[i],"_ATC")
  my_ATC<-(my_dt_MED$medicinal_product_atc_code[my_rows])
  
  
  
  
  my_df<-paste0(names[i],"_df")
  my_df<-as.data.frame(cbind(my_ID,my_Date, my_ATC))
  colnames(my_df)<-c("person_id", "date", "ATC")
  
  fwrite(my_df, paste0(my_write_path, names[i],".csv"))
  
  }
}

ATC_detect(my_data = my_dt_MED, names = names_drug_groups, ATC = ATC_groups)

