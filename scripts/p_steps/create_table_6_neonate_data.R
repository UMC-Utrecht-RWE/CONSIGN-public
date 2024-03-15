# table 5 group

# start with table 5a, maternal death (no modeling, numbers too small)

# need the cases and the pregnant matched controls
# maternal outcomes
# matneral covariates
# covid comorbidities


# need to pull in severity 

# import maternal outcome data
case_neo_outcome<-fread(paste0(output_neonates_case, "case_neonates_outcomes.csv"))
case_neo_outcome<-case_neo_outcome[,-("case_neonates_outcomes")]
case_mom_data<-fread(paste0(case_neonate_folder,"case_neonates.csv"))

case_mat_cov<-fread(paste0(g_output_mat_cov_pan_pos, "cases.csv"))
case_mat_cov$mom_id<-case_mat_cov$person_id

case_mat_cov<-case_mat_cov[,c("mom_id", "covid_trimester", "severity")]
# neonates need trimester and mom id from their moms
case_mom_data<-merge(case_mom_data, case_mat_cov, by="mom_id")

case_neo_outcome<-merge(case_mom_data, case_neo_outcome, by="person_id")


case_neo_outcome<-case_neo_outcome[case_neo_outcome$severity==0,]

# CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
# DAP<-CDM_source$data_access_provider_name
# if(DAP=="TEST"){
#   source(paste0(pre_dir, "/impute_NEO_DU_data.R"))
# }



T1_case_neo_outcome<-case_neo_outcome[case_neo_outcome$covid_trimester==1,]


T2_case_neo_outcome<-case_neo_outcome[case_neo_outcome$covid_trimester==2,]


T3_case_neo_outcome<-case_neo_outcome[case_neo_outcome$covid_trimester==3,]

