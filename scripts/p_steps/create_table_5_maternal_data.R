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
  
control_mat_outcome<-fread(paste0(g_output_mat_out_pan_neg, "covid_negative_pregnant_control.csv"))
control_preg_data<-fread(paste0(matched_folder, "matches_pregnant_cov_neg.csv"))
control_preg_data$gest_weeks<-((control_preg_data$pregnancy_end_date)-(control_preg_data$pregnancy_start_date))/7  
#control gest_cov_age needs to be calculated
control_preg_data$gest_age_cov<- (control_preg_data$covid_date)-(control_preg_data$pregnancy_start_date)


# import maternal covariate data and compute "any" column

case_mat_cov<-fread(paste0(g_output_mat_cov_pan_pos, "cases.csv"))
case_mat_cov$SA_SB<-(case_mat_cov$Spont_Abort+ case_mat_cov$Still_Birth)
case_mat_cov$SA_SB[case_mat_cov$SA_SB>0]<-1
case_mat_cov<-case_mat_cov[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]

  # create "any maternality" column

    case_mat_cov$any_mat<-rowSums(case_mat_cov[,4:ncol(case_mat_cov)])
    case_mat_cov$any_mat[case_mat_cov$any_mat>0]<-1
   

control_mat_cov<-fread(paste0(g_output_mat_cov_pan_neg, "covid_negative_pregnant_control.csv"))
control_mat_cov$SA_SB<-(control_mat_cov$Spont_Abort+control_mat_cov$Still_Birth)
control_mat_cov$SA_SB[control_mat_cov$SA_SB>0]<-1
control_mat_cov<-control_mat_cov[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]

    # create "any maternality" column
    
    control_mat_cov$any_mat<-rowSums(control_mat_cov[,4:ncol(control_mat_cov)])
    control_mat_cov$any_mat[control_mat_cov$any_mat>0]<-1

########################################################################
# import covid comorbidity data

case_cov_comorb<-fread(paste0(output_cov_cases, "cases.csv"))
case_cov_comorb$any_cov<-rowSums(case_cov_comorb[,4:ncol(case_cov_comorb)])
case_cov_comorb$any_cov[case_cov_comorb$any_cov>0]<-1

control_cov_comorb<-fread(paste0(output_cov_pregnant_control, "pregnant_cov_neg_control.csv"))
control_cov_comorb$any_cov<-rowSums(control_cov_comorb[,4:ncol(control_cov_comorb)])
control_cov_comorb$any_cov[control_cov_comorb$any_cov>0]<-1

###################################################################
# create "cohort" variable for modelling


case_other_vars<-merge(case_mat_cov[,c("person_id", "any_mat")], 
                       case_cov_comorb[,c("person_id","any_cov")], by="person_id")

case_other_vars<- merge(case_other_vars, case_preg_data[,c("person_id","gest_weeks", "type_of_pregnancy_end", "gest_age_cov")], by="person_id")

case_mat_outcome<-merge(case_mat_outcome, case_other_vars, by="person_id")
case_mat_outcome$cohort<-1


control_other_vars<-merge(control_mat_cov[,c("person_id", "any_mat")], 
                       control_cov_comorb[,c("person_id","any_cov")], by="person_id")

control_other_vars<- merge(control_other_vars, control_preg_data[,c("person_id","gest_weeks", "type_of_pregnancy_end", "gest_age_cov")], by="person_id")

control_mat_outcome<-merge(control_mat_outcome, control_other_vars, by="person_id")
control_mat_outcome$cohort<-0

# CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
# DAP<-CDM_source$data_access_provider_name
# if(DAP=="TEST"){
#   source(paste0(pre_dir, "/impute_model_data.R"))
# }

# SEPARTE OUT TRIMESTERS AND SEVERITY

# maternal outcomes

T1_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==1,]
T1_S_case_mat_outcome<-T1_case_mat_outcome[T1_case_mat_outcome$severity==1,]
T1_NS_case_mat_outcome<-T1_case_mat_outcome[T1_case_mat_outcome$severity==0,]

T2_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==2,]
T2_S_case_mat_outcome<-T2_case_mat_outcome[T2_case_mat_outcome$severity==1,]
T2_NS_case_mat_outcome<-T2_case_mat_outcome[T2_case_mat_outcome$severity==0,]

T3_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==3,]
T3_S_case_mat_outcome<-T3_case_mat_outcome[T3_case_mat_outcome$severity==1,]
T3_NS_case_mat_outcome<-T3_case_mat_outcome[T3_case_mat_outcome$severity==0,]


T1_control_mat_outcome<-control_mat_outcome[control_mat_outcome$covid_trimester==1,]
T2_control_mat_outcome<-control_mat_outcome[control_mat_outcome$covid_trimester==2,]
T3_control_mat_outcome<-control_mat_outcome[control_mat_outcome$covid_trimester==3,]

############################################3
# spont abort 

T2_SA_case_mat_outcome<-case_mat_outcome[(case_mat_outcome$covid_trimester==2 &(case_mat_outcome$gest_age_cov<(22*7))),]
T2_SA_S_case_mat_outcome<-T2_SA_case_mat_outcome[T2_SA_case_mat_outcome$severity==1,]
T2_SA_NS_case_mat_outcome<-T2_SA_case_mat_outcome[T2_SA_case_mat_outcome$severity==0,]

T2_SA_control_mat_outcome<-control_mat_outcome[(control_mat_outcome$covid_trimester==2 &(control_mat_outcome$gest_age_cov<(22*7))),]

#at least 20 gest_weeks

# maternal outcomes

T1_20_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==1)&(case_mat_outcome$gest_weeks>=20)),]
T1_20S_case_mat_outcome<-T1_20_case_mat_outcome[T1_20_case_mat_outcome$severity==1,]
T1_20NS_case_mat_outcome<-T1_20_case_mat_outcome[T1_20_case_mat_outcome$severity==0,]

T2_20_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==2)&(case_mat_outcome$gest_weeks>=20)),]
T2_20S_case_mat_outcome<-T2_20_case_mat_outcome[T2_20_case_mat_outcome$severity==1,]
T2_20NS_case_mat_outcome<-T2_20_case_mat_outcome[T2_20_case_mat_outcome$severity==0,]

T3_20_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==3)&(case_mat_outcome$gest_weeks>=20)),]
T3_20S_case_mat_outcome<-T3_20_case_mat_outcome[T3_20_case_mat_outcome$severity==1,]
T3_20NS_case_mat_outcome<-T3_20_case_mat_outcome[T3_20_case_mat_outcome$severity==0,]


T1_20_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==1)&(control_mat_outcome$gest_weeks>=20)),]
T2_20_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==2)&(control_mat_outcome$gest_weeks>=20)),]
T3_20_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==3)&(control_mat_outcome$gest_weeks>=20)),]



############################################3
#22 gest_weeks

# maternal outcomes

T1_22_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==1)&(case_mat_outcome$gest_weeks>=22)),]
T1_22S_case_mat_outcome<-T1_22_case_mat_outcome[T1_22_case_mat_outcome$severity==1,]
T1_22NS_case_mat_outcome<-T1_22_case_mat_outcome[T1_22_case_mat_outcome$severity==0,]

T2_22_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==2)&(case_mat_outcome$gest_weeks>=22)),]
T2_22S_case_mat_outcome<-T2_22_case_mat_outcome[T2_22_case_mat_outcome$severity==1,]
T2_22NS_case_mat_outcome<-T2_22_case_mat_outcome[T2_22_case_mat_outcome$severity==0,]

T2_22_SA_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==2)&(case_mat_outcome$gest_weeks>=22)&(case_mat_outcome$gest_age_cov<(22*7))),]
T2_22S_SA_case_mat_outcome<-T2_22_case_mat_outcome[T2_22_case_mat_outcome$severity==1,]
T2_22NS_SA_case_mat_outcome<-T2_22_case_mat_outcome[T2_22_case_mat_outcome$severity==0,]


T3_22_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==3)&(case_mat_outcome$gest_weeks>=22)),]
T3_22S_case_mat_outcome<-T3_22_case_mat_outcome[T3_22_case_mat_outcome$severity==1,]
T3_22NS_case_mat_outcome<-T3_22_case_mat_outcome[T3_22_case_mat_outcome$severity==0,]


T1_22_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==1)&(control_mat_outcome$gest_weeks>=22)),]
T2_22_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==2)&(control_mat_outcome$gest_weeks>=22)),]
T3_22_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==3)&(control_mat_outcome$gest_weeks>=22)),]


############################################3
#Live Birth

# maternal outcomes

T1_LB_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==1)&(case_mat_outcome$type_of_pregnancy_end=="LB")),]
T1_LB_S_case_mat_outcome<-T1_LB_case_mat_outcome[T1_LB_case_mat_outcome$severity==1,]
T1_LB_NS_case_mat_outcome<-T1_LB_case_mat_outcome[T1_LB_case_mat_outcome$severity==0,]

T2_LB_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==2)&(case_mat_outcome$type_of_pregnancy_end=="LB")),]
T2_LB_S_case_mat_outcome<-T2_LB_case_mat_outcome[T2_LB_case_mat_outcome$severity==1,]
T2_LB_NS_case_mat_outcome<-T2_LB_case_mat_outcome[T2_LB_case_mat_outcome$severity==0,]

# covid infection must occur before week 37
T3_LB_case_mat_outcome<-case_mat_outcome[((case_mat_outcome$covid_trimester==3)&(case_mat_outcome$type_of_pregnancy_end=="LB")& case_mat_outcome$gest_age_cov<(37*7)),]
T3_LB_S_case_mat_outcome<-T3_LB_case_mat_outcome[T3_LB_case_mat_outcome$severity==1,]
T3_LB_NS_case_mat_outcome<-T3_LB_case_mat_outcome[T3_LB_case_mat_outcome$severity==0,]


T1_LB_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==1)&(control_mat_outcome$type_of_pregnancy_end=="LB")),]
T2_LB_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==2)&(control_mat_outcome$type_of_pregnancy_end=="LB")),]
T3_LB_control_mat_outcome<-control_mat_outcome[((control_mat_outcome$covid_trimester==3)&(control_mat_outcome$type_of_pregnancy_end=="LB")),]


