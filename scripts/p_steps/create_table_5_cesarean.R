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


