# COMBINE CASE AND CONTROL DATA FOR MODELING

# need tests to confirm enough data present




preterm_output_df<-as.data.frame(matrix(ncol=5, nrow=12))

colnames(preterm_output_df)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# preterm trim1", "# preg trim1", "% trim1","Relative Risk 95%CI", 
           "# preterm trim2", "# preg trim2", "% trim2", "Relative Risk 95%CI",
           "# preterm trim3", "# preg trim3", "% trim3" ,"Relative Risk 95%CI")

preterm_output_df$names<-my_rows


trim1<-list()

trim1[[1]]<-sum(T1_LB_case_mat_outcome$PRETERM)
trim1[[2]]<-length(T1_LB_case_mat_outcome$PRETERM)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

preterm_output_df[1:3,2]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_LB_S_case_mat_outcome$PRETERM)
trim1[[2]]<-length(T1_LB_S_case_mat_outcome$PRETERM)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

preterm_output_df[1:3,3]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_LB_NS_case_mat_outcome$PRETERM)
trim1[[2]]<-length(T1_LB_NS_case_mat_outcome$PRETERM)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

preterm_output_df[1:3,4]<-unlist(trim1)


trim1_model<-list()


if(nrow(T1_LB_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_LB_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_LB_case_mat_outcome$PRETERM
  case_model_data$cohort<-T1_LB_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_LB_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_LB_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T1_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{PRETERM_all_results<-"insufficient data"}

trim1_model[[1]]<-PRETERM_all_results

# severe to match 
if(nrow(T1_LB_S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_LB_S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_LB_S_case_mat_outcome$PRETERM
  case_model_data$cohort<-T1_LB_S_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_LB_S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_LB_S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T1_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{PRETERM_severe_results<-"insufficient data"}

trim1_model[[2]]<-PRETERM_severe_results
# nonsevere to match 

if(nrow(T1_LB_NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_LB_NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_LB_NS_case_mat_outcome$PRETERM
  case_model_data$cohort<-T1_LB_NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_LB_NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_LB_NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T1_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{PRETERM_non_severe_results<-"insufficient data"}

trim1_model[[3]]<-PRETERM_non_severe_results
trim1_model[[4]]<-NA

preterm_output_df[4,2:5]<-unlist(trim1_model)



trim2<-list()

trim2[[1]]<-sum(T2_LB_case_mat_outcome$PRETERM)
trim2[[2]]<-length(T2_LB_case_mat_outcome$PRETERM)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

preterm_output_df[5:7,2]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_LB_S_case_mat_outcome$PRETERM)
trim2[[2]]<-length(T2_LB_S_case_mat_outcome$PRETERM)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

preterm_output_df[5:7,3]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_LB_NS_case_mat_outcome$PRETERM)
trim2[[2]]<-length(T2_LB_NS_case_mat_outcome$PRETERM)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

preterm_output_df[5:7,4]<-unlist(trim2)



trim2_model<-list()


if(nrow(T2_LB_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_LB_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_LB_case_mat_outcome$PRETERM
  case_model_data$cohort<-T2_LB_case_mat_outcome$cohort
  case_model_data$any_cov<-T2_LB_case_mat_outcome$any_cov
  case_model_data$any_mat<-T2_LB_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T2_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T2_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T2_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{PRETERM_all_results<-"insufficient data"}

trim2_model[[1]]<-PRETERM_all_results

# severe to match 
if(nrow(T2_LB_S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_LB_S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_LB_S_case_mat_outcome$PRETERM
  case_model_data$cohort<-T2_LB_S_case_mat_outcome$cohort
  case_model_data$any_cov<-T2_LB_S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T2_LB_S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T2_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T2_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T2_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{PRETERM_severe_results<-"insufficient data"}

trim2_model[[2]]<-PRETERM_severe_results
# nonsevere to match 

if(nrow(T2_LB_NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_LB_NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_LB_NS_case_mat_outcome$PRETERM
  case_model_data$cohort<-T2_LB_NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T2_LB_NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T2_LB_NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T2_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T2_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T2_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{PRETERM_non_severe_results<-"insufficient data"}

trim2_model[[3]]<-PRETERM_non_severe_results
trim2_model[[4]]<-NA

preterm_output_df[8,2:5]<-unlist(trim2_model)


trim3<-list()

trim3[[1]]<-sum(T3_LB_case_mat_outcome$PRETERM)
trim3[[2]]<-length(T3_LB_case_mat_outcome$PRETERM)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

preterm_output_df[9:11,2]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3_LB_S_case_mat_outcome$PRETERM)
trim3[[2]]<-length(T3_LB_S_case_mat_outcome$PRETERM)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

preterm_output_df[9:11,3]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3_NS_case_mat_outcome$PRETERM)
trim3[[2]]<-length(T3_NS_case_mat_outcome$PRETERM)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

preterm_output_df[9:11,4]<-unlist(trim3)

trim3_model<-list()


if(nrow(T3_LB_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_LB_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3_LB_case_mat_outcome$PRETERM
  case_model_data$cohort<-T3_LB_case_mat_outcome$cohort
  case_model_data$any_cov<-T3_LB_case_mat_outcome$any_cov
  case_model_data$any_mat<-T3_LB_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T3_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T3_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T3_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{PRETERM_all_results<-"insufficient data"}

trim3_model[[1]]<-PRETERM_all_results

# severe to match 
if(nrow(T3_LB_S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_LB_S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3_LB_S_case_mat_outcome$PRETERM
  case_model_data$cohort<-T3_LB_S_case_mat_outcome$cohort
  case_model_data$any_cov<-T3_LB_S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T3_LB_S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T3_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T3_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T3_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{PRETERM_severe_results<-"insufficient data"}

trim3_model[[2]]<-PRETERM_severe_results
# nonsevere to match 

if(nrow(T3_NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3_NS_case_mat_outcome$PRETERM
  case_model_data$cohort<-T3_NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T3_NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T3_NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_LB_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_LB_control_mat_outcome$PRETERM
  control_model_data$cohort<-T3_LB_control_mat_outcome$cohort
  control_model_data$any_cov<-T3_LB_control_mat_outcome$any_cov
  control_model_data$any_mat<-T3_LB_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  PRETERM_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  PRETERM_output<-coeftest(PRETERM_model, vcov = sandwich)
  print(PRETERM_output)
  est<-round(exp(PRETERM_output[2,1]),2)
  lo<-round(exp((PRETERM_output[2,1])-(1.96*PRETERM_output[2,2])),2)
  hi<-round(exp((PRETERM_output[2,1])+(1.96*PRETERM_output[2,2])),2)
  PRETERM_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{PRETERM_non_severe_results<-"insufficient data"}

trim3_model[[3]]<-PRETERM_non_severe_results
trim3_model[[4]]<-NA

preterm_output_df[12,2:5]<-unlist(trim3_model)

#######################################################################
#controls

trim1<-list()

trim1[[1]]<-sum(T1_LB_control_mat_outcome$PRETERM)
trim1[[2]]<-length(T1_LB_control_mat_outcome$PRETERM)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

preterm_output_df[1:3,5]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_LB_control_mat_outcome$PRETERM)
trim2[[2]]<-length(T2_LB_control_mat_outcome$PRETERM)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

preterm_output_df[5:7,5]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_LB_control_mat_outcome$PRETERM)
trim3[[2]]<-length(T3_LB_control_mat_outcome$PRETERM)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

preterm_output_df[9:11,5]<-unlist(trim3)

fwrite(preterm_output_df, paste0(final_output_dir,"table_5_preterm.csv"))


