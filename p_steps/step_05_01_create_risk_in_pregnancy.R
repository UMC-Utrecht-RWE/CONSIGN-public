# load D3_included_pregnancies  
load(paste0(dirtemp,"D3_groups_of_pregnancies.RData"))

# Keep the highest quality record for each group of pregnancy
# ordering 
D3_gop<-D3_groups_of_pregnancies[order(person_id,group_identifier, order_quality, -record_date),]
# creating record number for each group of pregnancy
D3_gop<-D3_gop[,n:=seq_along(.I), by=.(group_identifier, person_id, highest_quality)]
#creating unique identifier for each group of pregnancy
D3_gop<-D3_gop[,pers_group_id:=paste0(person_id,"_", group_identifier_colored)]
# keeping the first record 
D3_pregnancy_outcomes<-D3_gop[n==1, .(person_id, 
                                      pers_group_id, 
                                      survey_id, 
                                      highest_quality, 
                                      PROMPT, 
                                      pregnancy_start_date, 
                                      pregnancy_end_date, 
                                      type_of_pregnancy_end )]
# create the risk
concept_sets_of_our_study_risk <- list("DP_CVD", "DP_COVSICKLE", "DP_COVCOPD", 
                                       "DP_COVDIAB", "DP_COVOBES", "DP_COVCKD", 
                                       "DP_COVHIV", "IMMUNOSUPPR", "DP_COVCANCER",
                                       "COVSICKLE", "COVCOPD", "COVDIAB", 
                                       "COVOBES", "COVCKD", "COVHIV", "COVCANCER", 
                                       "HF_narrow", "HF_possible", "MYOCARD_narrow",
                                       "MYOCARD_possible", "CAD_narrow", "CAD_possible")

#load concepts datasets
for (conceptvar in concept_sets_of_our_study_risk){
  load(paste0(dirtemp,conceptvar,".RData")) 
}
# create dummy for medication
D3_risk_in_pregnancy <- D3_pregnancy_outcomes
for (concept in concept_sets_of_our_study_risk) {
  print(paste0("Creating risk: ", concept))
  
  risk <- MergeFilterAndCollapse( listdatasetL = list(get(concept)),
                                  datasetS = D3_pregnancy_outcomes, 
                                  typemerge = 2,
                                  key = c("person_id"), 
                                  condition = "date >= (pregnancy_start_date - 365) & date <= pregnancy_start_date",
                                  strata=c("pers_group_id"),
                                  summarystat = list(list(c("exist"), "date" , concept)))
  
  D3_risk_in_pregnancy <- merge(D3_risk_in_pregnancy, 
                                risk, 
                                by = c("pers_group_id"), 
                                all.x = T)
  setnames(D3_risk_in_pregnancy, concept, "risk_to_be_renamed")
  D3_risk_in_pregnancy <- D3_risk_in_pregnancy[is.na(risk_to_be_renamed), risk_to_be_renamed := 0]
  #create "at_risk" = 1 if at least one risk is = 1
  D3_risk_in_pregnancy <- D3_risk_in_pregnancy[risk_to_be_renamed == 1, at_risk := 1]
  setnames(D3_risk_in_pregnancy, "risk_to_be_renamed", concept)
} 
D3_risk_in_pregnancy <- D3_risk_in_pregnancy[is.na(at_risk), at_risk :=0]

# D3_risk_in_pregnancy[, .N]
# for (concept in concept_sets_of_our_study_risk){
#   print(paste0(concept, ": ", D3_risk_in_pregnancy[get(concept)==1, .N]))
# }

D3_pregnancy_with_risk <- D3_risk_in_pregnancy[, -c("DP_CVD", "DP_COVSICKLE", "DP_COVCOPD", 
                                                   "DP_COVDIAB", "DP_COVOBES", "DP_COVCKD", 
                                                   "DP_COVHIV", "IMMUNOSUPPR", "DP_COVCANCER",
                                                   "COVSICKLE", "COVCOPD", "COVDIAB", 
                                                   "COVOBES", "COVCKD", "COVHIV", "COVCANCER", 
                                                   "HF_narrow", "HF_possible", "MYOCARD_narrow",
                                                   "MYOCARD_possible", "CAD_narrow", "CAD_possible")]

save(D3_pregnancy_with_risk, file=paste0(dirtemp,"D3_pregnancy_with_risk.RData"))

rm(D3_gop, D3_groups_of_pregnancies, D3_pregnancy_outcomes, 
   D3_risk_in_pregnancy, risk, D3_pregnancy_with_risk)

for (i in concept_sets_of_our_study_risk) {
  rm( list = i)
}
