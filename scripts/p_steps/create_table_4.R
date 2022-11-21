# table 4 historical table

# DENMARK ONLY HISTORICAL
# don't source the other cohorts

# for historical group, no outcome (no pandemic pregnancy to have an outcome from)--> covariate is the event from historical pregnancies from the last 2 years

hist_maternal_events<-fread(paste0(g_output_mat_cov_hist, "historical.csv"))


hist_preg<-fread(paste0(hist_preg_folder, "my_PREG.csv"))
hist_preg$gest_weeks<-(hist_preg$pregnancy_end_date-hist_preg$pregnancy_start_date)/7

hist_neonates<-fread(paste0(output_neonates_hist, "hist_neonates_outcomes.csv"))


my_rownames<- c("Maternal death", 
"Gestational diabetes",
"Pre-eclampsia (and eclampsia )",
"Spontaneous abortion",
"(TOPFA)",
"Caesarean section",
"Pre-term birth",
"Stillbirth",

"Neonatal death" ,
"Low birth weight", 
"Small-for-gestational age (SGA)/ Intrauterine growth restriction (IUGR)" ,
"Low apgar score (5-minute)" ,
"Major congenital anomalies",
"Microcephaly")

denom_all<-nrow(hist_preg)
denom_20_weeks<-sum(as.numeric(hist_preg$gest_weeks>=20))

denom_22_weeks<-sum(as.numeric(hist_preg$gest_weeks>=22))

denom_LB<-sum(as.numeric(hist_preg$type_of_pregnancy_end=="LB"))

mat_death<-sum(hist_maternal_events$maternal_death)
gest_diab<-sum(hist_maternal_events$gest_diab)
preeclamp<-sum(hist_maternal_events$Preeclampsia)
spontabort<-sum(hist_maternal_events$Spont_Abort)
topfa<-sum(hist_maternal_events$TOPFA)
casearean<-sum(hist_maternal_events$CESAREA)
preterm<-sum(hist_maternal_events$PRETERM)
still_birth<-sum(hist_maternal_events$Still_Birth)


neonatal_death<-sum(hist_neonates$NEONATAL_death)
LBW<-sum(hist_neonates$LBW)
FGA<-sum(hist_neonates$SGA_FGR)
apgar<-sum(hist_neonates$APGARLOW)
majorca<-sum(hist_neonates$MAJORCA)
micro<-sum(hist_neonates$MICROCEPHALY)

denominators<-c(denom_all, denom_20_weeks, denom_20_weeks, denom_all, denom_all, denom_all, denom_LB, denom_22_weeks, rep(denom_LB,6))

outcomes<-c(mat_death, gest_diab, preeclamp,  spontabort, topfa, casearean,preterm,still_birth, neonatal_death, LBW, FGA, apgar, majorca, micro)
my_perc<-vector()

for(i in 1:length(outcomes)){
  raw<-prop.test(outcomes[i], denominators[i])
  perc<-(round(raw$estimate,5))*100
  lo<-(round(raw$conf.int[1],5))*100
  hi<-(round(raw$conf.int[2],5))*100
  result<-paste0(perc," (",lo,"-", hi,")")
  my_perc[i]<-result
}

table4<-as.data.frame(cbind(my_rownames, denominators, outcomes, my_perc))

colnames(table4)<-c("outcome", "denominator", "count", "percent and CI")

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name


fwrite(table4, paste0(final_output_dir, DAP,"_table_4.csv"))
