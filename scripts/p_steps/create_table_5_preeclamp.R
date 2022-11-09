# COMBINE CASE AND CONTROL DATA FOR MODELING

# need tests to confirm enough data present

# start with CASAEREAN: straight forward


Preeclampsia_output<-as.data.frame(matrix(ncol=5, nrow=4))

colnames(Preeclampsia_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# Preeclampsia trim1", "# preg trim1", "% trim1","Relative Risk 95%CI")

Preeclampsia_output$names<-my_rows


trim1<-list()

trim1[[1]]<-sum(T1_20_case_mat_outcome$Preeclampsia)
trim1[[2]]<-length(T1_20_case_mat_outcome$Preeclampsia)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

Preeclampsia_output[1:3,2]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_20S_case_mat_outcome$Preeclampsia)
trim1[[2]]<-length(T1_20S_case_mat_outcome$Preeclampsia)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

Preeclampsia_output[1:3,3]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_20NS_case_mat_outcome$Preeclampsia)
trim1[[2]]<-length(T1_20NS_case_mat_outcome$Preeclampsia)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

Preeclampsia_output[1:3,4]<-unlist(trim1)


trim1_model<-list()


if(nrow(T1_20_case_mat_outcome)>0){
  
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_20_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_20_case_mat_outcome$Preeclampsia
  case_model_data$cohort<-T1_20_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_20_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_20_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_20_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_20_control_mat_outcome$Preeclampsia
  control_model_data$cohort<-T1_20_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_20_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_20_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  Preeclampsia_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  Preeclam_output<-coeftest(Preeclampsia_model, vcov = sandwich)
  print(Preeclam_output)
  est<-round(exp(Preeclam_output[2,1]),2)
  lo<-round(exp((Preeclam_output[2,1])-(1.96*Preeclam_output[2,2])),2)
  hi<-round(exp((Preeclam_output[2,1])+(1.96*Preeclam_output[2,2])),2)
  Preeclampsia_all_results<-paste0(est, " (", lo,"-",hi,")")
}else{Preeclampsia_all_results<-"insufficient data"}

trim1_model[[1]]<-Preeclampsia_all_results

# severe to match 
if(nrow(T1_20S_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_20S_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_20S_case_mat_outcome$Preeclampsia
  case_model_data$cohort<-T1_20S_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_20S_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_20S_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_20_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_20_control_mat_outcome$Preeclampsia
  control_model_data$cohort<-T1_20_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_20_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_20_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  Preeclampsia_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  Preeclam_output<-coeftest(Preeclampsia_model, vcov = sandwich)
  print(Preeclam_output)
  est<-round(exp(Preeclam_output[2,1]),2)
  lo<-round(exp((Preeclam_output[2,1])-(1.96*Preeclam_output[2,2])),2)
  hi<-round(exp((Preeclam_output[2,1])+(1.96*Preeclam_output[2,2])),2)
  Preeclampsia_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{Preeclampsia_severe_results<-"insufficient data"}

trim1_model[[2]]<-Preeclampsia_severe_results
# nonsevere to match 

if(nrow(T1_20NS_case_mat_outcome)>0){
  case_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_20NS_case_mat_outcome)))
  colnames(case_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  case_model_data$outcome<-T1_20NS_case_mat_outcome$Preeclampsia
  case_model_data$cohort<-T1_20NS_case_mat_outcome$cohort
  case_model_data$any_cov<-T1_20NS_case_mat_outcome$any_cov
  case_model_data$any_mat<-T1_20NS_case_mat_outcome$any_mat
  
  
  control_model_data<-as.data.frame(matrix(ncol=4, nrow=nrow(T1_20_control_mat_outcome)))
  colnames(control_model_data)<-c("outcome", "cohort", "any_cov", "any_mat")
  
  control_model_data$outcome<-T1_20_control_mat_outcome$Preeclampsia
  control_model_data$cohort<-T1_20_control_mat_outcome$cohort
  control_model_data$any_cov<-T1_20_control_mat_outcome$any_cov
  control_model_data$any_mat<-T1_20_control_mat_outcome$any_mat
  
  model_data<-rbind(case_model_data, control_model_data)
  
  Preeclampsia_model<-glm(formula=(outcome~cohort+any_cov+any_mat), data=model_data,family = "poisson")
  Preeclam_output<-coeftest(Preeclampsia_model, vcov = sandwich)
  print(Preeclam_output)
  est<-round(exp(Preeclam_output[2,1]),2)
  lo<-round(exp((Preeclam_output[2,1])-(1.96*Preeclam_output[2,2])),2)
  hi<-round(exp((Preeclam_output[2,1])+(1.96*Preeclam_output[2,2])),2)
  Preeclampsia_non_severe_results<-paste0(est, " (", lo,"-",hi,")")
  
}else{Preeclampsia_non_severe_results<-"insufficient data"}

trim1_model[[3]]<-Preeclampsia_non_severe_results
trim1_model[[4]]<-NA

Preeclampsia_output[4,2:5]<-unlist(trim1_model)

# controls 

trim1<-list()

trim1[[1]]<-sum(T1_control_mat_outcome$Preeclampsia)
trim1[[2]]<-length(T1_control_mat_outcome$Preeclampsia)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

Preeclampsia_output[1:3,5]<-unlist(trim1)




fwrite(Preeclampsia_output, paste0(final_output_dir,"table_5_Preeclampsia.csv"))


