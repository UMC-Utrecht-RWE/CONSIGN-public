# table 5 group

# start with table 5a, maternal death (no modeling, numbers too small)

# need the cases and the pregnant matched controls
# maternal outcomes
# matneral covariates
# covid comorbidities

# glm(formula = outcome ~ case + cov_comorb+ mat_cov, family = "poisson", data = p)


# import maternal outcome data
case_neo_outcome<-fread(paste0(output_neonates_case, "case_neonates_outcomes.csv"))
case_mom_data<-fread(paste0(case_neonate_folder,"case_neonates.csv"))

neo_vars<-colnames(case_neo_outcome[,2:ncol(case_neo_outcome)])

control_neo_outcome<-fread(paste0(output_neonates_control, "control_neonates_outcomes.csv"))
control_mom_data<-fread(paste0(control_neonate_folder,"control_neonates.csv"))
# import maternal covariate data and compute "any" column

case_mat_cov<-fread(paste0(g_output_mat_cov_pan_pos, "cases.csv"))
case_mat_cov<-case_mat_cov[case_mat_cov$person_id%in%case_mom_data$mom_id,]

case_mat_cov$SA_SB<-(case_mat_cov$Spont_Abort+ case_mat_cov$Still_Birth)
case_mat_cov$SA_SB[case_mat_cov$SA_SB>0]<-1
case_mat_cov<-case_mat_cov[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]

  # create "any maternality" column

    case_mat_cov$any_mat<-rowSums(case_mat_cov[,4:ncol(case_mat_cov)])
    case_mat_cov$any_mat[case_mat_cov$any_mat>0]<-1
    # need to link mother cov data to child
   case_mat_cov$mom_id<-case_mat_cov$person_id

control_mat_cov<-fread(paste0(g_output_mat_cov_pan_neg, "covid_negative_pregnant_control.csv"))
control_mat_cov<-control_mat_cov[control_mat_cov$person_id%in%control_mom_data$mom_id,]
control_mat_cov$SA_SB<-(control_mat_cov$Spont_Abort+control_mat_cov$Still_Birth)
control_mat_cov$SA_SB[control_mat_cov$SA_SB>0]<-1
control_mat_cov<-control_mat_cov[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]

    # create "any maternality" column
    
    control_mat_cov$any_mat<-rowSums(control_mat_cov[,4:ncol(control_mat_cov)])
    control_mat_cov$any_mat[control_mat_cov$any_mat>0]<-1
    # need to link mother cov data to child
    control_mat_cov$mom_id<-control_mat_cov$person_id
    

########################################################################
# import covid comorbidity data

case_cov_comorb<-fread(paste0(output_cov_cases, "cases.csv"))
case_cov_comorb<-case_cov_comorb[case_cov_comorb$person_id%in%case_mom_data$mom_id,]    
case_cov_comorb$any_cov<-rowSums(case_cov_comorb[,4:ncol(case_cov_comorb)])
case_cov_comorb$any_cov[case_cov_comorb$any_cov>0]<-1
# need to link mother cov data to child
case_cov_comorb$mom_id<-case_cov_comorb$person_id

control_cov_comorb<-fread(paste0(output_cov_pregnant_control, "pregnant_cov_neg_control.csv"))
control_cov_comorb<-control_cov_comorb[control_cov_comorb$person_id%in%control_mom_data$mom_id,]  
control_cov_comorb$any_cov<-rowSums(control_cov_comorb[,4:ncol(control_cov_comorb)])
control_cov_comorb$any_cov[control_cov_comorb$any_cov>0]<-1
# need to link mother cov data to child
control_cov_comorb$mom_id<-control_cov_comorb$person_id

###################################################################
# create "cohort" variable for modelling

case_neo_outcome$cohort<-1
case_cov_comorb$cohort<-1
case_mat_cov$cohort<-1

control_cov_comorb$cohort<-0
control_mat_cov$cohort<-0
control_neo_outcome$cohort<-0

# neonates need trimester and severity from their moms

case_mom_link<-merge(case_mom_data, case_cov_comorb[,c("mom_id", "covid_trimester", "severity")], by="mom_id")
case_neo_outcome<-merge(case_mom_link, case_neo_outcome, by="person_id")
case_neo_outcome<-merge(case_neo_outcome, case_mat_cov[,c('mom_id', "any_mat")], by="mom_id")
case_neo_outcome<-merge(case_neo_outcome, case_cov_comorb[,c('mom_id', "any_cov")], by="mom_id")

control_mom_link<-merge(control_mom_data, control_cov_comorb[,c("mom_id", "covid_trimester", "severity")], by="mom_id")
control_neo_outcome<-merge(control_mom_link, control_neo_outcome, by="person_id")
control_neo_outcome<-merge(control_neo_outcome, control_mat_cov[,c('mom_id', "any_mat")], by="mom_id")
control_neo_outcome<-merge(control_neo_outcome, control_cov_comorb[,c('mom_id', "any_cov")], by="mom_id")

# SEPARTE OUT TRIMESTERS AND SEVERITY

# maternal outcomes

T1_case_neo_outcome<-case_neo_outcome[case_neo_outcome$covid_trimester==1,]
T1S_case_neo_outcome<-T1_case_neo_outcome[T1_case_neo_outcome$severity==1,]
T1NS_case_neo_outcome<-T1_case_neo_outcome[T1_case_neo_outcome$severity==0,]

T2_case_neo_outcome<-case_neo_outcome[case_neo_outcome$covid_trimester==2,]
T2S_case_neo_outcome<-T2_case_neo_outcome[T2_case_neo_outcome$severity==1,]
T2NS_case_neo_outcome<-T2_case_neo_outcome[T2_case_neo_outcome$severity==0,]

T3_case_neo_outcome<-case_neo_outcome[case_neo_outcome$covid_trimester==3,]
T3S_case_neo_outcome<-T3_case_neo_outcome[T3_case_neo_outcome$severity==1,]
T3NS_case_neo_outcome<-T3_case_neo_outcome[T3_case_neo_outcome$severity==0,]


T1_control_neo_outcome<-control_neo_outcome[control_neo_outcome$covid_trimester==1,]
T2_control_neo_outcome<-control_neo_outcome[control_neo_outcome$covid_trimester==2,]
T3_control_neo_outcome<-control_neo_outcome[control_neo_outcome$covid_trimester==3,]

