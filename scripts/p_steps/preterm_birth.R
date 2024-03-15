# get one maternal covariate -PRETERM- for report
CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name


preg_cohort_folders<-list(hist_preg_folder,preg_match_folder, cases_match_folder)
output_folders<-list(output_mat_cov_hist, output_mat_cov_pan_neg, output_mat_cov_pan_pos)

my_preg_data<-c("my_PREG.csv", "preg_trim_cov.csv", "preg_trim_cov.csv")


# only events within 1 year before covid+ pregnancy start date

for(i in 1:length(preg_cohort_folders)){
  
  cohort_folder<-unlist(preg_cohort_folders[i])
  output_folder<-unlist(output_folders[i])
  preg_data<-my_preg_data[i]
  df_preg<-fread(paste0(cohort_folder,preg_data))

if(DAP=="TEST"){
  df_preg$gest_weeks<-sample(c(30,35, 40,42), nrow(df_preg), replace=T)
  df_preg$type_of_pregnancy_end<-sample(c("LB","LB", "LB", "SA","SB"),nrow(df_preg), replace = T )
  
}
  
df_preg$gest_weeks<-(df_preg$pregnancy_end_date-df_preg$pregnancy_start_date)/7

PRETERM_ID<-df_preg$person_id[(df_preg$type_of_pregnancy_end=="LB")&(df_preg$gest_weeks<37)]
PRETERM_Date<-df_preg$pregnancy_end_date[(df_preg$type_of_pregnancy_end=="LB")&(df_preg$gest_weeks<37)]


PRETERM_cov<-as.data.frame(cbind(PRETERM_ID,PRETERM_Date))
colnames(PRETERM_cov)<-c("id","date")
fwrite(PRETERM_cov, paste0(output_folder,"PRETERM.csv"))
}