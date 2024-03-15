# table 5 group

# start with table 5a, maternal death (no modeling, numbers too small)

# need the cases and the pregnant matched controls
# maternal outcomes
# matneral covariates
# covid comorbidities

# glm(formula = outcome ~ case + cov_comorb+ mat_cov, family = "poisson", data = p)


# import maternal outcome data
case_mat_outcome<-fread(paste0(g_output_mat_out_pan_pos, "cases.csv"))

control_mat_outcome<-fread(paste0(g_output_mat_out_pan_neg, "covid_negative_pregnant_control.csv"))

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

case_mat_outcome$cohort<-1
case_cov_comorb$cohort<-1
case_mat_cov$cohort<-1

control_cov_comorb$cohort<-0
control_mat_cov$cohort<-0
control_mat_outcome$cohort<-0

# SEPARTE OUT TRIMESTERS AND SEVERITY

# maternal outcomes

T1_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==1,]
T1S_case_mat_outcome<-T1_case_mat_outcome[T1_case_mat_outcome$severity==1,]
T1NS_case_mat_outcome<-T1_case_mat_outcome[T1_case_mat_outcome$severity==0,]

T2_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==2,]
T2S_case_mat_outcome<-T2_case_mat_outcome[T2_case_mat_outcome$severity==1,]
T2NS_case_mat_outcome<-T2_case_mat_outcome[T2_case_mat_outcome$severity==0,]

T3_case_mat_outcome<-case_mat_outcome[case_mat_outcome$covid_trimester==3,]
T3S_case_mat_outcome<-T3_case_mat_outcome[T3_case_mat_outcome$severity==1,]
T3NS_case_mat_outcome<-T3_case_mat_outcome[T3_case_mat_outcome$severity==0,]


T1_control_mat_outcome<-control_mat_outcome[control_mat_outcome$covid_trimester==1,]
T2_control_mat_outcome<-control_mat_outcome[control_mat_outcome$covid_trimester==2,]
T3_control_mat_outcome<-control_mat_outcome[control_mat_outcome$covid_trimester==3,]

# maternal covariate data

T1_case_mat_cov<-case_mat_cov[case_mat_cov$covid_trimester==1,]
T1S_case_mat_cov<-T1_case_mat_cov[T1_case_mat_cov$severity==1,]
T1NS_case_mat_cov<-T1_case_mat_cov[T1_case_mat_cov$severity==0,]

T2_case_mat_cov<-case_mat_cov[case_mat_cov$covid_trimester==2,]
T2S_case_mat_cov<-T2_case_mat_cov[T2_case_mat_cov$severity==1,]
T2NS_case_mat_cov<-T2_case_mat_cov[T2_case_mat_cov$severity==0,]

T3_case_mat_cov<-case_mat_cov[case_mat_cov$covid_trimester==3,]
T3S_case_mat_cov<-T3_case_mat_cov[T3_case_mat_cov$severity==1,]
T3NS_case_mat_cov<-T3_case_mat_cov[T3_case_mat_cov$severity==0,]


T1_control_mat_cov<-control_mat_cov[control_mat_cov$covid_trimester==1,]
T2_control_mat_cov<-control_mat_cov[control_mat_cov$covid_trimester==2,]
T3_control_mat_cov<-control_mat_cov[control_mat_cov$covid_trimester==3,]



# covid comorbidity data

T1_case_cov_comorb<-case_cov_comorb[case_cov_comorb$covid_trimester==1,]
T1S_case_cov_comorb<-T1_case_cov_comorb[T1_case_cov_comorb$severity==1,]
T1NS_case_cov_comorb<-T1_case_cov_comorb[T1_case_cov_comorb$severity==0,]

T2_case_cov_comorb<-case_cov_comorb[case_cov_comorb$covid_trimester==2,]
T2S_case_cov_comorb<-T2_case_cov_comorb[T2_case_cov_comorb$severity==1,]
T2NS_case_cov_comorb<-T2_case_cov_comorb[T2_case_cov_comorb$severity==0,]

T3_case_cov_comorb<-case_cov_comorb[case_cov_comorb$covid_trimester==3,]
T3S_case_cov_comorb<-T3_case_cov_comorb[T3_case_cov_comorb$severity==1,]
T3NS_case_cov_comorb<-T3_case_cov_comorb[T3_case_cov_comorb$severity==0,]


T1_control_cov_comorb<-control_cov_comorb[control_cov_comorb$covid_trimester==1,]
T2_control_cov_comorb<-control_cov_comorb[control_cov_comorb$covid_trimester==2,]
T3_control_cov_comorb<-control_cov_comorb[control_cov_comorb$covid_trimester==3,]

# COMBINE CASE AND CONTROL DATA FOR MODELING

# need tests to confirm enough data present

# start with CASAEREAN: straight forward

if(nrow(T1_case_cov_comorb)>0){

case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_case_cov_comorb)))
colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")

case_model_data$outcome<-T1_case_mat_outcome$CESAREA
case_model_data$cohort<-T1_case_mat_outcome$cohort
case_model_data$any_cov<-T1_case_cov_comorb$any_cov
case_model_data$any_mat<-T1_case_mat_cov$any_mat


control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_cov_comorb)))
colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")

control_model_data$outcome<-T1_control_mat_outcome$CESAREA
control_model_data$cohort<-T1_control_mat_outcome$cohort
control_model_data$any_cov<-T1_control_cov_comorb$any_cov
control_model_data$any_mat<-T1_control_mat_cov$any_mat

model_data<-rbind(case_model_data, control_model_data)

CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
CESAREA_output<-summary(CESAREA_model)
CESAREA_est<-CESAREA_output$coefficients[2,1]
CESAREA_SD<-CESAREA_output$coefficients[2,2]
CESAREA_P<-CESAREA_output$coefficients[2,4]


}else{CESAREA_all_results<-"insufficient data"}

# severe to match 
if(nrow(T1S_case_cov_comorb)>0){
case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1S_case_cov_comorb)))
colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")

case_model_data$outcome<-T1S_case_mat_outcome$CESAREA
case_model_data$cohort<-T1S_case_mat_outcome$cohort
case_model_data$any_cov<-T1S_case_cov_comorb$any_cov
case_model_data$any_mat<-T1S_case_mat_cov$any_mat


control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_cov_comorb)))
colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")

control_model_data$outcome<-T1_control_mat_outcome$CESAREA
control_model_data$cohort<-T1_control_mat_outcome$cohort
control_model_data$any_cov<-T1_control_cov_comorb$any_cov
control_model_data$any_mat<-T1_control_mat_cov$any_mat

model_data<-rbind(case_model_data, control_model_data)

CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
CESAREA_output<-summary(CESAREA_model)
CESAREA_est<-CESAREA_output$coefficients[2,1]
CESAREA_SD<-CESAREA_output$coefficients[2,2]
CESAREA_P<-CESAREA_output$coefficients[2,4]



}else{CESAREA_severe_results<-"insufficient data"}

# nonsevere to match 

if(nrow(T1NS_case_cov_comorb)>0){
case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1NS_case_cov_comorb)))
colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")

case_model_data$outcome<-T1NS_case_mat_outcome$CESAREA
case_model_data$cohort<-T1NS_case_mat_outcome$cohort
case_model_data$any_cov<-T1NS_case_cov_comorb$any_cov
case_model_data$any_mat<-T1NS_case_mat_cov$any_mat


control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_cov_comorb)))
colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")

control_model_data$outcome<-T1_control_mat_outcome$CESAREA
control_model_data$cohort<-T1_control_mat_outcome$cohort
control_model_data$any_cov<-T1_control_cov_comorb$any_cov
control_model_data$any_mat<-T1_control_mat_cov$any_mat

model_data<-rbind(case_model_data, control_model_data)

CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
CESAREA_output<-summary(CESAREA_model)
CESAREA_est<-CESAREA_output$coefficients[2,1]
CESAREA_SD<-CESAREA_output$coefficients[2,2]
CESAREA_P<-CESAREA_output$coefficients[2,4]



}else{CESAREA_non_severe_results<-"insufficient data"}


