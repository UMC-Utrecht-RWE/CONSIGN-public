# load pregnancies dataset
load(paste0(dirtemp,"D3_pregnancy_risk_trimester.RData"))
#load concepts datasets
for (conceptvar in concept_sets_of_our_study_medications_vaccines){
  load(paste0(dirtemp,conceptvar,".RData")) 
}

setnames(D3_pregnancy_risk_trimester, "date", "covid_date")
# create dummy for medication
D3_medication_in_pregnancy <- D3_pregnancy_risk_trimester

for (concept in concept_sets_of_our_study_medications_vaccines) {
  print(paste0("Creating dummy for medication: ", concept))
  MFC_medication <- MergeFilterAndCollapse( listdatasetL = list(get(concept)),
                                            datasetS = D3_pregnancy_risk_trimester, 
                                            typemerge = 2,
                                            key = c("person_id"), 
                                            condition = "date >= start_trim & date <= end_trim",
                                            strata=c("pers_group_id", "trim"),
                                            summarystat = list(list(c("exist"), "date" , concept)))
  
  D3_medication_in_pregnancy <- merge(D3_medication_in_pregnancy, 
                                      MFC_medication, 
                                      by = c("pers_group_id", "trim"), 
                                      all.x = T)
  
  # setnames(D3_medication_in_pregnancy, concept, "risk_to_be_renamed")
  # D3_medication_in_pregnancy <- D3_medication_in_pregnancy[is.na(risk_to_be_renamed), risk_to_be_renamed := 0]
  # setnames(D3_medication_in_pregnancy, "risk_to_be_renamed", concept)
  
}   

# D3_medication_in_pregnancy[, .N]
# unique(D3_medication_in_pregnancy, by = "pers_group_id")[,.N]
# for (concept in concept_sets_of_our_study_medications_vaccines){
#   D3_medication_in_pregnancy <- D3_medication_in_pregnancy[, temp:=NA]
#   D3_medication_in_pregnancy <- D3_medication_in_pregnancy[get(concept)==1, temp :=1][is.na(temp), temp:=0]
#   D3_medication_in_pregnancy <- D3_medication_in_pregnancy[, temp:= max(temp), by = "pers_group_id"]
# 
#   DF_unique <- unique(D3_medication_in_pregnancy, by = "pers_group_id")
#   print(paste0(concept, " Number: ", D3_medication_in_pregnancy[get(concept)==1, .N]))
#   print(paste0(concept, " Pregnancy: ", round(DF_unique[temp==1, .N]/DF_unique[, .N], 6)))
#   setnames(D3_medication_in_pregnancy, "temp", paste0(concept, "_in_preg"))
# }

# Clean the dataset
CONSIGN_analytical_dataset <- D3_medication_in_pregnancy

CONSIGN_analytical_dataset<-CONSIGN_analytical_dataset[,n:=seq_along(.I), by=.(pers_group_id)]
CONSIGN_analytical_dataset<-CONSIGN_analytical_dataset[n==1, preg_id := paste0("ARS_id_", seq_along(1:nrow(CONSIGN_analytical_dataset[n==1])))]
CONSIGN_analytical_dataset<-CONSIGN_analytical_dataset[is.na(preg_id), preg_id:="Z"]
CONSIGN_analytical_dataset<-CONSIGN_analytical_dataset[,preg_id := min(preg_id), by = "pers_group_id"]

CONSIGN_analytical_dataset <- CONSIGN_analytical_dataset[, covid_month := format(covid_date, format = "%B%Y")]
setnames(CONSIGN_analytical_dataset, "status", "covid_status")
CONSIGN_analytical_dataset <- CONSIGN_analytical_dataset[, covid_status := as.character(covid_status)]
CONSIGN_analytical_dataset <- CONSIGN_analytical_dataset[covid_status == 0, covid_status:= "No COVID-19 in this trimester or prior trimester"]
CONSIGN_analytical_dataset <- CONSIGN_analytical_dataset[covid_status == 1, covid_status:= "COVID-19 in this trimester"]
CONSIGN_analytical_dataset <- CONSIGN_analytical_dataset[covid_status == 2, covid_status:= "COVID-19 prior trimester"]

setnames(CONSIGN_analytical_dataset, "age_category", "age_group")


CONSIGN_analytical_dataset <- CONSIGN_analytical_dataset[, `:=`(id = NA, 
                                                                preg_start = NA, 
                                                                preg_end = NA, 
                                                                start_trim = NA, 
                                                                end_trim = NA, 
                                                                covid_date = NA)]

CONSIGN_analytical_dataset_ARS <- CONSIGN_analytical_dataset[, .(id, 
                                                                 preg_id,
                                                                 preg_start, 
                                                                 preg_end,
                                                                 trim,
                                                                 start_trim, 
                                                                 end_trim,
                                                                 end_trim_in_2021,
                                                                 prior_to_COVID,
                                                                 covid_date, 
                                                                 covid_month,
                                                                 covid_in_trim,
                                                                 covid_status,
                                                                 at_risk,
                                                                 age,
                                                                 age_group,
                                                                 C0_2_3_4_7_8_9,
                                                                 B01,
                                                                 J05,
                                                                 J01,
                                                                 J02,
                                                                 J04,
                                                                 J06,
                                                                 J07, 
                                                                 N02, 
                                                                 N05, 
                                                                 N06,
                                                                 A10,
                                                                 H02,
                                                                 L03,
                                                                 L04,
                                                                 M01,
                                                                 R01,
                                                                 R03,
                                                                 R05)]


CONSIGN_analytical_dataset_ARS <- CONSIGN_analytical_dataset_ARS[prior_to_COVID == 0]

save(D3_medication_in_pregnancy, file=paste0(dirtemp,"D3_medication_in_pregnancy.RData"))
fwrite(CONSIGN_analytical_dataset_ARS, paste0(direxp,"CONSIGN_analytical_dataset_ARS.csv"))

rm(D3_pregnancy_risk_trimester, D3_medication_in_pregnancy, MFC_medication,
   CONSIGN_analytical_dataset_ARS,CONSIGN_analytical_dataset)
for (conceptvar in concept_sets_of_our_study_medications_vaccines){
  rm(list = conceptvar) 
}
