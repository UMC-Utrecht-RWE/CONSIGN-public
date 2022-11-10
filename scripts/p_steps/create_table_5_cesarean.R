# COMBINE CASE AND CONTROL DATA FOR MODELING

# need tests to confirm enough data present

# start with CASAEREAN: straight forward

print("hail cesar!   ;)")

cesarean_output<-as.data.frame(matrix(ncol=5, nrow=12))

colnames(cesarean_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# cesarean trim1", "# preg trim1", "% trim1","Relative Risk 95%CI", 
           "# cesarean trim2", "# preg trim2", "% trim2", "Relative Risk 95%CI",
           "# cesarean trim3", "# preg trim3", "% trim3" ,"Relative Risk 95%CI")

cesarean_output$names<-my_rows


trim1<-list()

trim1[[1]]<-sum(T1_case_mat_outcome$CESAREA)
trim1[[2]]<-length(T1_case_mat_outcome$CESAREA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

cesarean_output[1:3,2]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_S_case_mat_outcome$CESAREA)
trim1[[2]]<-length(T1_S_case_mat_outcome$CESAREA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

cesarean_output[1:3,3]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_NS_case_mat_outcome$CESAREA)
trim1[[2]]<-length(T1_NS_case_mat_outcome$CESAREA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

cesarean_output[1:3,4]<-unlist(trim1)


trim1_model<-list()


if(nrow(T1_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_case_mat_outcome$CESAREA
  case_model_data$cohort<-T1_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_mat_outcome$CESAREA
  control_model_data$cohort<-T1_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{CESAREA_all_results<-"insufficient data"}

trim1_model[[1]]<-CESAREA_all_results

# severe to match 
if(nrow(T1_S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_S_case_mat_outcome$CESAREA
  case_model_data$cohort<-T1_S_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_mat_outcome$CESAREA
  control_model_data$cohort<-T1_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{CESAREA_severe_results<-"insufficient data"}

trim1_model[[2]]<-CESAREA_severe_results
# nonsevere to match 

if(nrow(T1_NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_NS_case_mat_outcome$CESAREA
  case_model_data$cohort<-T1_NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_mat_outcome$CESAREA
  control_model_data$cohort<-T1_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{CESAREA_non_severe_results<-"insufficient data"}

trim1_model[[3]]<-CESAREA_non_severe_results
trim1_model[[4]]<-NA

cesarean_output[4,2:5]<-unlist(trim1_model)



trim2<-list()

trim2[[1]]<-sum(T2_case_mat_outcome$CESAREA)
trim2[[2]]<-length(T2_case_mat_outcome$CESAREA)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

cesarean_output[5:7,2]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_S_case_mat_outcome$CESAREA)
trim2[[2]]<-length(T2_S_case_mat_outcome$CESAREA)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

cesarean_output[5:7,3]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_NS_case_mat_outcome$CESAREA)
trim2[[2]]<-length(T2_NS_case_mat_outcome$CESAREA)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

cesarean_output[5:7,4]<-unlist(trim2)



trim2_model<-list()


if(nrow(T2_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_case_mat_outcome$CESAREA
  case_model_data$cohort<-T2_case_mat_outcome$cohort
  case_model_data$any_cov<-T2_case_mat_outcome$any_cov
  case_model_data$any_mat<-T2_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_control_mat_outcome$CESAREA
  control_model_data$cohort<-T2_control_mat_outcome$cohort
  control_model_data$any_cov<-T2_control_mat_outcome$any_cov
  control_model_data$any_mat<-T2_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{CESAREA_all_results<-"insufficient data"}

trim2_model[[1]]<-CESAREA_all_results

# severe to match 
if(nrow(T2_S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_S_case_mat_outcome$CESAREA
  case_model_data$cohort<-T2_S_case_mat_outcome$cohort
  case_model_data$any_cov<-T2_S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T2_S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_control_mat_outcome$CESAREA
  control_model_data$cohort<-T2_control_mat_outcome$cohort
  control_model_data$any_cov<-T2_control_mat_outcome$any_cov
  control_model_data$any_mat<-T2_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{CESAREA_severe_results<-"insufficient data"}

trim2_model[[2]]<-CESAREA_severe_results
# nonsevere to match 

if(nrow(T2_NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_NS_case_mat_outcome$CESAREA
  case_model_data$cohort<-T2_NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T2_NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T2_NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_control_mat_outcome$CESAREA
  control_model_data$cohort<-T2_control_mat_outcome$cohort
  control_model_data$any_cov<-T2_control_mat_outcome$any_cov
  control_model_data$any_mat<-T2_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{CESAREA_non_severe_results<-"insufficient data"}

trim2_model[[3]]<-CESAREA_non_severe_results
trim2_model[[4]]<-NA

cesarean_output[8,2:5]<-unlist(trim2_model)


trim3<-list()

trim3[[1]]<-sum(T3_case_mat_outcome$CESAREA)
trim3[[2]]<-length(T3_case_mat_outcome$CESAREA)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

cesarean_output[9:11,2]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3_S_case_mat_outcome$CESAREA)
trim3[[2]]<-length(T3_S_case_mat_outcome$CESAREA)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

cesarean_output[9:11,3]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3_NS_case_mat_outcome$CESAREA)
trim3[[2]]<-length(T3_NS_case_mat_outcome$CESAREA)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

cesarean_output[9:11,4]<-unlist(trim3)

trim3_model<-list()


if(nrow(T3_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3_case_mat_outcome$CESAREA
  case_model_data$cohort<-T3_case_mat_outcome$cohort
  case_model_data$any_cov<-T3_case_mat_outcome$any_cov
  case_model_data$any_mat<-T3_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_control_mat_outcome$CESAREA
  control_model_data$cohort<-T3_control_mat_outcome$cohort
  control_model_data$any_cov<-T3_control_mat_outcome$any_cov
  control_model_data$any_mat<-T3_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{CESAREA_all_results<-"insufficient data"}

trim3_model[[1]]<-CESAREA_all_results

# severe to match 
if(nrow(T3_S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3_S_case_mat_outcome$CESAREA
  case_model_data$cohort<-T3_S_case_mat_outcome$cohort
  case_model_data$any_cov<-T3_S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T3_S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_control_mat_outcome$CESAREA
  control_model_data$cohort<-T3_control_mat_outcome$cohort
  control_model_data$any_cov<-T3_control_mat_outcome$any_cov
  control_model_data$any_mat<-T3_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{CESAREA_severe_results<-"insufficient data"}

trim3_model[[2]]<-CESAREA_severe_results
# nonsevere to match 

if(nrow(T3NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3NS_case_mat_outcome$CESAREA
  case_model_data$cohort<-T3NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T3NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T3NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_control_mat_outcome$CESAREA
  control_model_data$cohort<-T3_control_mat_outcome$cohort
  control_model_data$any_cov<-T3_control_mat_outcome$any_cov
  control_model_data$any_mat<-T3_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  CESAREA_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  CESAREA_output<-coeftest(CESAREA_model, vcov = sandwich)
  print(CESAREA_output)
  est<-round(exp(CESAREA_output[2,1]),2)
  lo<-round(exp((CESAREA_output[2,1])-(1.96*CESAREA_output[2,2])),2)
  hi<-round(exp((CESAREA_output[2,1])+(1.96*CESAREA_output[2,2])),2)
  CESAREA_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{CESAREA_non_severe_results<-"insufficient data"}

trim3_model[[3]]<-CESAREA_non_severe_results
trim3_model[[4]]<-NA

cesarean_output[12,2:5]<-unlist(trim3_model)

#######################################################################
#controls

trim1<-list()

trim1[[1]]<-sum(T1_control_mat_outcome$CESAREA)
trim1[[2]]<-length(T1_control_mat_outcome$CESAREA)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

cesarean_output[1:3,5]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_control_mat_outcome$CESAREA)
trim2[[2]]<-length(T2_control_mat_outcome$CESAREA)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

cesarean_output[5:7,5]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_control_mat_outcome$CESAREA)
trim3[[2]]<-length(T3_control_mat_outcome$CESAREA)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

cesarean_output[9:11,5]<-unlist(trim3)

fwrite(cesarean_output, paste0(final_output_dir,"table_5_cesarean.csv"))


