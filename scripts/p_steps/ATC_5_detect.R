#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#8/4/2022
#update 13/6/2022
#major revision 20/9/22 (made into function+machine readable input)

#CONSIGN

#identify use of medicines over time for women of reproduction age (later sorted into cohorts)

my_path<-preselect_folder

my_write_path<- raw_atc_5_counts

#pattern match to find and loop through all the MEDS tables


my_dt_MED<-IMPORT_PATTERN(pat="MEDICINES", dir = my_path)


df <- select(my_dt_MED, date_dispensing, date_prescription)


drug_date<-df %>% transmute(Label = coalesce(date_dispensing, date_prescription))

my_dt_MED$drug_date<-drug_date

rm(df)
rm(drug_date)

# translate SAP ATC table into machine-readable format

names_drug_groups<-list( "Enoxaparin","Heparin_Unfractionated", "Acetylsalicyclic_Acid", 
                         "GC_dexamethasone", "GC_betamethasone","GC_hydrocortisone",
                         "GC_methylprednisolone","GC_prednsisone",
                        "AB_amoxicillin","AB_ampicillin", "AB_ampicillin_combination",
                        "AB_piperacilline_BLI","AB_ampicillin_BLI","AB_amoxixillin_BLI",
                        "AB_1st_Ceph_cefalexin","AB_1st_Ceph_cefazolin","AB_2nd_Ceph_cefuroxime",
                        "AB_3rd_Ceph_cefotaxime","AB_3rd_Ceph_ceftazidime","AB_3rd_Ceph_ceftriaxone",
                        "AB_3rd_Ceph_cefixime","AB_3rd_Ceph_cefodizime","AB_4th_Ceph_cefepime",
                        "AB_Carbapenems_meropenem", 
                        "AB_Macrolides_azithromycin","AB_Macrolides_clarithromycin","AB_Macrolides_erythromycin",
                        "AB_Lincosamides_clindamycin",
                        "AB_Anminoglycosides_amikacin","AB_Anminoglycosides_gentamicin",
                        "AB_Glycopeptide_vancomycin","AB_Glycopeptide_teicoplanin",
                        "AV_Nucleosides_remdesivir", "AV_HIV_lopinavir_ritonavir",
                        "AV_Neuraminidase_oseltamivir", "AV_HCV_ribavirin", "AV_Other_favipiravir",
                        "IG_Normal_Human_IV", "Vaccinations_Viral_COVID19","Antineoplastic_BCR_ABL_TKI_imatinib",
                        "Antineoplastic_JAK_inhibitors_ruxolitinib",
                        "Immunosuppressants_baricitinib","Immunosuppressants_tofacitinib",
                        "Immunosuppressants_IL1_tocilizumab","Immunosuppressants_IL1_sarilumab",
                        "Immunosuppressants_IL6_anakinra","Immunosuppressants_IL6_canakinumab",
                        "Immunosuppressants_Calcineurin_ciclosporin",
                        "Antigout_colchicine", "Analgesics_Paracetemol",
                        "Psychoanalepts_SSRI_fluvoxamine", "Antiprotozoals_Amoebiasis_metronidazole", 
                        "Antiprotozoals_Amoebiasis_nitazoxanide",
                        "Antiprotozoals_Malaria_chloroquine","Antiprotozoals_Malaria_hydroxychloroquine",
                        
                        "Antinematodal_ivermectin", "Obstructive_Airway_salbutamol")

ATC_groups<-list( "B01AB01" ,"B01AB05" ,"B01AC06" ,"H02AB02","H02AB01","H02AB09","H02AB04","H02AB06",
                  "J01CA04","J01CA01","J01CA51", "J01CR05","J01CR01","J01CR02",
                  "J01DB01","J01DB04", "J01DC02","J01DD01","J01DD02","J01DD04","J01DD08","J01DD09",
                  "J01DE01","J01DH02","J01FA10","J01FA09","J01FA01",
                  "J01FF01","J01GB06","J01GB03","J01XA01","J01XA02",
                  "J05AB16", "J05AR10" , "J05AH02" , "J05AP01" , "J05AX27" , 
                  "J06BA02", "J07BX03" ,  "L01EA01", "L01EJ01", "L04AA37","L04AA29",
                  "L04AC07","L04AC14","L04AC03","L04AC08","L04AD01",
                  "M04AC01", "N02BE01",
                  "N06AB08" , "P01AB01","P01AX11","P01BA01","P01BA02", "P02CF01",   "R03AC02" )

# bind to data_frame to check that codes are aligned correctly
df_atc_groups<-as.data.frame(cbind((names_drug_groups), (ATC_groups)))

colnames(df_atc_groups)<-c("name", "ATC")

print(df_atc_groups)

fwrite(df_atc_groups, paste0(my_write_path,"/source_data.csv"))
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

