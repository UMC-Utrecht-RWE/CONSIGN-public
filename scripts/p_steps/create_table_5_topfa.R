# COMBINE CASE AND CONTROL DATA FOR MODELING

# need tests to confirm enough data present

# start with CASAEREAN: straight forward


topfa_output<-as.data.frame(matrix(ncol=5, nrow=4))

colnames(topfa_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# topfa trim1", "# preg trim1", "% trim1","Relative Risk 95%CI")

topfa_output$names<-my_rows


trim1<-list()

trim1[[1]]<-sum(T1_case_mat_outcome$TOPFA)
trim1[[2]]<-length(T1_case_mat_outcome$TOPFA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

topfa_output[1:3,2]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_S_case_mat_outcome$TOPFA)
trim1[[2]]<-length(T1_S_case_mat_outcome$TOPFA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

topfa_output[1:3,3]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T2_NS_case_mat_outcome$TOPFA)
trim1[[2]]<-length(T2_NS_case_mat_outcome$TOPFA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

topfa_output[1:3,4]<-unlist(trim1)


trim1_model<-list()


if(nrow(T1_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_case_mat_outcome$TOPFA
  case_model_data$cohort<-T1_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_mat_outcome$TOPFA
  control_model_data$cohort<-T1_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  TOPFA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  TOPFA_output<-coeftest(TOPFA_model, vcov = sandwich)
  print(TOPFA_output)
  est<-round(exp(TOPFA_output[2,1]),2)
  lo<-round(exp((TOPFA_output[2,1])-(1.96*TOPFA_output[2,2])),2)
  hi<-round(exp((TOPFA_output[2,1])+(1.96*TOPFA_output[2,2])),2)
  TOPFA_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{TOPFA_all_results<-"insufficient data"}

trim1_model[[1]]<-TOPFA_all_results

# severe to match 
if(nrow(T1_S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_S_case_mat_outcome$TOPFA
  case_model_data$cohort<-T1_S_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_mat_outcome$TOPFA
  control_model_data$cohort<-T1_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  TOPFA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  TOPFA_output<-coeftest(TOPFA_model, vcov = sandwich)
  print(TOPFA_output)
  est<-round(exp(TOPFA_output[2,1]),2)
  lo<-round(exp((TOPFA_output[2,1])-(1.96*TOPFA_output[2,2])),2)
  hi<-round(exp((TOPFA_output[2,1])+(1.96*TOPFA_output[2,2])),2)
  TOPFA_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{TOPFA_severe_results<-"insufficient data"}

trim1_model[[2]]<-TOPFA_severe_results
# nonsevere to match 

if(nrow(T2_NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_NS_case_mat_outcome$TOPFA
  case_model_data$cohort<-T2_NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T2_NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T2_NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_mat_outcome$TOPFA
  control_model_data$cohort<-T1_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  TOPFA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  TOPFA_output<-coeftest(TOPFA_model, vcov = sandwich)
  print(TOPFA_output)
  est<-round(exp(TOPFA_output[2,1]),2)
  lo<-round(exp((TOPFA_output[2,1])-(1.96*TOPFA_output[2,2])),2)
  hi<-round(exp((TOPFA_output[2,1])+(1.96*TOPFA_output[2,2])),2)
  TOPFA_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{TOPFA_non_severe_results<-"insufficient data"}

trim1_model[[3]]<-TOPFA_non_severe_results
trim1_model[[4]]<-NA

topfa_output[4,2:5]<-unlist(trim1_model)

# controls 

trim1<-list()

trim1[[1]]<-sum(T1_control_mat_outcome$TOPFA)
trim1[[2]]<-length(T1_control_mat_outcome$TOPFA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

topfa_output[1:3,5]<-unlist(trim1)




fwrite(topfa_output, paste0(final_output_dir,"table_5_TOPFA.csv"))


