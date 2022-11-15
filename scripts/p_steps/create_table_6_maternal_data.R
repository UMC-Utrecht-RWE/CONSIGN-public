# table 5 group

# start with table 5a, maternal death (no modeling, numbers too small)

# need the cases and the pregnant matched controls
# maternal outcomes
# matneral covariates
# covid comorbidities

# glm(formula = outcome ~ case + cov_comorb+ mat_cov, family = "poisson", data = p)


# import maternal outcome data
case_mat_outcome<-fread(paste0(g_output_mat_out_pan_pos, "cases.csv"))
case_preg_data<-fread(paste0(matched_folder,"matches_cases.csv"))
case_preg_data$gest_weeks<-((case_preg_data$pregnancy_end_date)-(case_preg_data$pregnancy_start_date))/7  

case_mat_outcome<- merge(case_mat_outcome, case_preg_data[,c("person_id","gest_weeks", "type_of_pregnancy_end", "gest_age_cov")], by="person_id")

# import maternal covariate data and compute "any" column



CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name
if(DAP=="TEST"){
  source(paste0(pre_dir, "/impute_drug_util_data.R"))
}

# only non-severe for table 6

case_mat_outcome<-case_mat_outcome[case_mat_outcome$severity==0,]

# SEPARTE OUT TRIMESTERS AND SEVERITY

# maternal outcomes

T1_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==1,]


T2_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==2,]


T3_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==3,]

############################################3
#at least 20 gest_weeks

# maternal outcomes

T1_20_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==1)&(case_mat_outcome$gest_weeks>=20)),]

T2_20_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==2)&(case_mat_outcome$gest_weeks>=20)),]

T3_20_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==3)&(case_mat_outcome$gest_weeks>=20)),]

############################################3
#22 gest_weeks

# maternal outcomes

T1_22_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==1)&(case_mat_outcome$gest_weeks>=22)),]

T2_22_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==2)&(case_mat_outcome$gest_weeks>=22)),]

T3_22_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==3)&(case_mat_outcome$gest_weeks>=22)),]

############################################3
#Live Birth

# maternal outcomes

T1_LB_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==1)&(case_mat_outcome$type_of_pregnancy_end=="LB")),]

T2_LB_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==2)&(case_mat_outcome$type_of_pregnancy_end=="LB")),]

# covid infection must occur before week 37 
T3_LB_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==3)&(case_mat_outcome$type_of_pregnancy_end=="LB")& case_mat_outcome$gest_age_cov<(37*7)),]

