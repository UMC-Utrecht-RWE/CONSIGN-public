#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#8/4/2022
#update 13/6/2022
#major revision 20/9/22 (made into function+machine readable input)

#CONSIGN

#identify use of medicines over time for women of reproduction age (later sorted into cohorts)

my_path<-preselect_folder

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/atc_4_counts/total/")), dir.create(paste0(projectFolder, "/g_intermediate/atc_4_counts/total/")), FALSE))
raw_atc_4_counts_total    <- paste0(projectFolder, "/g_intermediate/atc_4_counts/total/")

my_write_path_total<- raw_atc_4_counts_total

#pattern match to find and loop through all the MEDS tables

actual_MED_tables<-list.files(my_path, pattern="MEDICINES")


# translate SAP ATC table into machine-readable format
names_drug_groups<-list( "Antithrombotic_Heparin","Gluco_Corticoisteroids",
                         "AB_Penicillin_Extended", "AB_Penicillin_Combination", 
                         "AB_1st_Cephalosporins","AB_2nd_Cephalosporins","AB_3rd_Cephalosporins",
                         "AB_4th_Cephalosporins",
                         "AB_Carbapenems", "AB_Macrolides","AB_Lincosamides",
                         "AB_Anminoglycosides_other", "AB_Glycopeptide",
                         "Antivirals_Nucleosides_Excl_RTI", "Antivirals_HIV","Antivirals_Neuraminidase",
                         "Antivirals_HCV", "Antivirals_Other",
                         "IG_Antiviral_monoclonal_antibodies", "IG_Normal_Human",
                         "Vaccinations_Viral_Other", "Antineoplastic_BCR_ABL_tyrosine_kinase_inhibitors",
                         "Antineoplastic_JAK_inhibitors",
                         "Immunostimulants_Interferons", "Immunosuppressants_Selective",
                         "Immunosuppressants_Interleukin","Immunosuppressants_Calcineurin",
                         "Antigout_no_effect_Uric", "Analgesics_Anilides",
                         "Psychoanaleptics_Antidepressants_SSRI", "Antiprotozoals_Amoebiasis_Nitroimidazole", 
                         "Antiprotozoals_Amoebiasis_Other",
                         "Antiprotozoals_Malaria_Aminoquinolines",
                         "Antinematodal_Avermectines", "Obstructive_Airway_Adrenergics_Beta2")

ATC_groups<-list( "B01AB" ,"H02AB", "J01CA", "J01CR","J01DB", "J01DC","J01DD","J01DE","J01DH",
                  "J01FA","J01FF","J01GB","J01XA",
                  "J05AB" , "J05AR" , "J05AH" , "J05AP" , "J05AX" , 
                  "J06BD","J06BA", "J07BX" ,  "L01EA", "L01EJ", "L03AB", "L04AA","L04AC","L04AD",
                  "M04AC", "N02BE",
                  "N06AB" , "P01AB","P01AX","P01BA", "P02CF",   "R03AC" )

# bind to data_frame to check that codes are aligned correctly
df_atc_groups<-as.data.frame(cbind((names_drug_groups), (ATC_groups)))

colnames(df_atc_groups)<-c("name", "ATC")

print(df_atc_groups)

fwrite(df_atc_groups, paste0(g_intermediate,"/atc_4_source_data.csv"))

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
  fwrite(my_df, paste0(my_write_path_total, names[i],j,".csv"))
  
  }
}


for(j in 1:length(actual_MED_tables)){
  
  my_dt_MED<-fread(paste0(my_path,actual_MED_tables[j]))
  
  my_dt_MED$drug_date<-my_dt_MED$date_dispensing
  
  ATC_detect(my_data = my_dt_MED, names = names_drug_groups, ATC = ATC_groups)

}

for(i in 1:length(names_drug_groups)){
  
  drug_group<-as.character(names_drug_groups[i])
  print(drug_group)
  
  my_data<-IMPORT_PATTERN(pat=drug_group, dir=my_write_path_total)
  
  fwrite(my_data, paste0(raw_atc_4_counts,names_drug_groups[i],".csv"))
}
