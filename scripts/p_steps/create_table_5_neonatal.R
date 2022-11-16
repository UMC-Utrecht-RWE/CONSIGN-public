# COMBINE CASE AND CONTROL DATA FOR MODELING

# need tests to confirm enough data present

# start with CASAEREAN: straight forward

for(i in 1:length(neo_vars)){
  my_var<-neo_vars[i]
  print(my_var)

my_output<-as.data.frame(matrix(ncol=5, nrow=12))

colnames(my_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# outcome trim1", "# preg trim1", "% trim1","Relative Risk 95%CI", 
           "# outcome trim2", "# preg trim2", "% trim2", "Relative Risk 95%CI",
           "# outcome trim3", "# preg trim3", "% trim3" ,"Relative Risk 95%CI")

my_output$names<-my_rows


fwrite(my_output, paste0(final_output_dir,"table_5_",my_var, ".csv"))

trim1<-list()

trim1[[1]]<-sum(T1_case_neo_outcome[[my_var]])
trim1[[2]]<-length(T1_case_neo_outcome[[my_var]])
if(trim1[[2]]>0){
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")
}else{trim1[[3]]<-"no observations"}
my_output[1:3,2]<-unlist(trim1)

trim1<-list()


trim1[[1]]<-sum(T1S_case_neo_outcome[[my_var]])
trim1[[2]]<-length(T1S_case_neo_outcome[[my_var]])
if(trim1[[2]]>0){
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")
}else{trim1[[3]]<-"no observations"}
my_output[1:3,3]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1NS_case_neo_outcome[[my_var]])
trim1[[2]]<-length(T1NS_case_neo_outcome[[my_var]])
if(trim1[[2]]>0){
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")
}else{trim1[[3]]<-"no observations"}
my_output[1:3,4]<-unlist(trim1)


trim1_model<-list()


if(nrow(T1_case_neo_outcome)>0 & nrow(T1_control_neo_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T1_case_neo_outcome$cohort
  case_model_data$any_cov<-T1_case_neo_outcome$any_cov
  case_model_data$any_mat<-T1_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T1_control_neo_outcome$cohort
  control_model_data$any_cov<-T1_control_neo_outcome$any_cov
  control_model_data$any_mat<-T1_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{my_outcome_all_results<-"insufficient data"}

trim1_model[[1]]<-my_outcome_all_results

# severe to match 
if(nrow(T1S_case_neo_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1S_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1S_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T1S_case_neo_outcome$cohort
  case_model_data$any_cov<-T1S_case_neo_outcome$any_cov
  case_model_data$any_mat<-T1S_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T1_control_neo_outcome$cohort
  control_model_data$any_cov<-T1_control_neo_outcome$any_cov
  control_model_data$any_mat<-T1_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{my_outcome_severe_results<-"insufficient data"}

trim1_model[[2]]<-my_outcome_severe_results
# nonsevere to match 

if(nrow(T1NS_case_neo_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1NS_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1NS_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T1NS_case_neo_outcome$cohort
  case_model_data$any_cov<-T1NS_case_neo_outcome$any_cov
  case_model_data$any_mat<-T1NS_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T1_control_neo_outcome$cohort
  control_model_data$any_cov<-T1_control_neo_outcome$any_cov
  control_model_data$any_mat<-T1_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{my_outcome_non_severe_results<-"insufficient data"}

trim1_model[[3]]<-my_outcome_non_severe_results
trim1_model[[4]]<-NA

my_output[4,2:5]<-unlist(trim1_model)



trim2<-list()

trim2[[1]]<-sum(T2_case_neo_outcome[[my_var]])
trim2[[2]]<-length(T2_case_neo_outcome[[my_var]])
if(trim2[[2]]>0){
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")
}else{trim2[[3]]<-"no observations"}
my_output[5:7,2]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2S_case_neo_outcome[[my_var]])
trim2[[2]]<-length(T2S_case_neo_outcome[[my_var]])
if(trim2[[2]]>0){
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")
}else{trim2[[3]]<-"no observations"}
my_output[5:7,3]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2NS_case_neo_outcome[[my_var]])
trim2[[2]]<-length(T2NS_case_neo_outcome[[my_var]])
if(trim2[[2]]>0){
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")
}else{trim2[[3]]<-"no observations"}
my_output[5:7,4]<-unlist(trim2)



trim2_model<-list()


if(nrow(T2_case_neo_outcome)>0 & nrow(T2_control_neo_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T2_case_neo_outcome$cohort
  case_model_data$any_cov<-T2_case_neo_outcome$any_cov
  case_model_data$any_mat<-T2_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T2_control_neo_outcome$cohort
  control_model_data$any_cov<-T2_control_neo_outcome$any_cov
  control_model_data$any_mat<-T2_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{my_outcome_all_results<-"insufficient data"}

trim2_model[[1]]<-my_outcome_all_results

# severe to match 
if(nrow(T2S_case_neo_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2S_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2S_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T2S_case_neo_outcome$cohort
  case_model_data$any_cov<-T2S_case_neo_outcome$any_cov
  case_model_data$any_mat<-T2S_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T2_control_neo_outcome$cohort
  control_model_data$any_cov<-T2_control_neo_outcome$any_cov
  control_model_data$any_mat<-T2_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{my_outcome_severe_results<-"insufficient data"}

trim2_model[[2]]<-my_outcome_severe_results
# nonsevere to match 

if(nrow(T2NS_case_neo_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2NS_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T2NS_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T2NS_case_neo_outcome$cohort
  case_model_data$any_cov<-T2NS_case_neo_outcome$any_cov
  case_model_data$any_mat<-T2NS_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T2_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T2_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T2_control_neo_outcome$cohort
  control_model_data$any_cov<-T2_control_neo_outcome$any_cov
  control_model_data$any_mat<-T2_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{my_outcome_non_severe_results<-"insufficient data"}

trim2_model[[3]]<-my_outcome_non_severe_results
trim2_model[[4]]<-NA

my_output[8,2:5]<-unlist(trim2_model)


trim3<-list()

trim3[[1]]<-sum(T3_case_neo_outcome[[my_var]])
trim3[[2]]<-length(T3_case_neo_outcome[[my_var]])
if(trim3[[2]]>0){
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")
}else{trim3[[3]]<-"no observations"}
my_output[9:11,2]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3S_case_neo_outcome[[my_var]])
trim3[[2]]<-length(T3S_case_neo_outcome[[my_var]])
if(trim3[[2]]>0){
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")
}else{trim3[[3]]<-"no observations"}
my_output[9:11,3]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3NS_case_neo_outcome[[my_var]])
trim3[[2]]<-length(T3NS_case_neo_outcome[[my_var]])
if(trim3[[2]]>0){
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")
}else{trim3[[3]]<-"no observations"}
my_output[9:11,4]<-unlist(trim3)

trim3_model<-list()


if(nrow(T3_case_neo_outcome)>0 & nrow(T3_control_neo_outcome)>0 ){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T3_case_neo_outcome$cohort
  case_model_data$any_cov<-T3_case_neo_outcome$any_cov
  case_model_data$any_mat<-T3_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T3_control_neo_outcome$cohort
  control_model_data$any_cov<-T3_control_neo_outcome$any_cov
  control_model_data$any_mat<-T3_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{my_outcome_all_results<-"insufficient data"}

trim3_model[[1]]<-my_outcome_all_results

# severe to match 
if(nrow(T3S_case_neo_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3S_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3S_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T3S_case_neo_outcome$cohort
  case_model_data$any_cov<-T3S_case_neo_outcome$any_cov
  case_model_data$any_mat<-T3S_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T3_control_neo_outcome$cohort
  control_model_data$any_cov<-T3_control_neo_outcome$any_cov
  control_model_data$any_mat<-T3_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{my_outcome_severe_results<-"insufficient data"}

trim3_model[[2]]<-my_outcome_severe_results
# nonsevere to match 

if(nrow(T3NS_case_neo_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3NS_case_neo_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T3NS_case_neo_outcome[[my_var]]
  case_model_data$cohort<-T3NS_case_neo_outcome$cohort
  case_model_data$any_cov<-T3NS_case_neo_outcome$any_cov
  case_model_data$any_mat<-T3NS_case_neo_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T3_control_neo_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T3_control_neo_outcome[[my_var]]
  control_model_data$cohort<-T3_control_neo_outcome$cohort
  control_model_data$any_cov<-T3_control_neo_outcome$any_cov
  control_model_data$any_mat<-T3_control_neo_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  my_outcome_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  my_outcome_output<-coeftest(my_outcome_model, vcov = sandwich)
  print(my_outcome_output)
  est<-round(exp(my_outcome_output[2,1]),2)
  lo<-round(exp((my_outcome_output[2,1])-(1.96*my_outcome_output[2,2])),2)
  hi<-round(exp((my_outcome_output[2,1])+(1.96*my_outcome_output[2,2])),2)
  my_outcome_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{my_outcome_non_severe_results<-"insufficient data"}

trim3_model[[3]]<-my_outcome_non_severe_results
trim3_model[[4]]<-NA

my_output[12,2:5]<-unlist(trim3_model)

#######################################################################
#controls

trim1<-list()

trim1[[1]]<-sum(T1_control_neo_outcome[[my_var]])
trim1[[2]]<-length(T1_control_neo_outcome[[my_var]])
if(trim1[[2]]>0){
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")
}else{trim1[[3]]<-"no observations"}
my_output[1:3,5]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_control_neo_outcome[[my_var]])
trim2[[2]]<-length(T2_control_neo_outcome[[my_var]])
if(trim2[[2]]>0){
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")
}else{trim2[[3]]<-"no observations"}
my_output[5:7,5]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_control_neo_outcome[[my_var]])
trim3[[2]]<-length(T3_control_neo_outcome[[my_var]])
if(trim3[[2]]>0){
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")
}else{trim3[[3]]<-"no observations"}
my_output[9:11,5]<-unlist(trim3)

fwrite(my_output, paste0(final_output_dir,"table_5_",my_var, ".csv"))
}


