# table 4 spontaneous abotion and still birth

# time to event

# add survival package to packages.R

# pull in case and preg control pregnancy data
# we need person_id, preg_start, preg_end, cov trimester, severity, type of end

case_preg<-fread(paste0(matched_folder, "matches_cases.csv"))
case_preg<-case_preg[,c("person_id" , "pregnancy_start_date" ,"pregnancy_end_date" ,"type_of_pregnancy_end" , "cov_trimester", "severity" )]


control_preg<-fread(paste0(matched_folder, "matches_pregnant_cov_neg.csv"))
control_preg<-control_preg[,c("person_id" , "pregnancy_start_date" ,"pregnancy_end_date" ,"type_of_pregnancy_end" , "cov_trimester", "severity" )]


# pull in the covariate data (previous adverse outcomes)
# create "any maternality" column

case_mat_cov<-fread(paste0(g_output_mat_cov_pan_pos, "cases.csv"))
case_mat_cov$SA_SB<-(case_mat_cov$Spont_Abort+ case_mat_cov$Still_Birth)
case_mat_cov$SA_SB[case_mat_cov$SA_SB>0]<-1
case_mat_cov<-case_mat_cov[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]
case_mat_cov$any_mat<-rowSums(case_mat_cov[,4:ncol(case_mat_cov)])
case_mat_cov$any_mat[case_mat_cov$any_mat>0]<-1
case_mat_cov<-case_mat_cov[,c("person_id", "any_mat")]


control_mat_cov<-fread(paste0(g_output_mat_cov_pan_neg, "covid_negative_pregnant_control.csv"))
control_mat_cov$SA_SB<-(control_mat_cov$Spont_Abort+control_mat_cov$Still_Birth)
control_mat_cov$SA_SB[control_mat_cov$SA_SB>0]<-1
control_mat_cov<-control_mat_cov[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]
control_mat_cov$any_mat<-rowSums(control_mat_cov[,4:ncol(control_mat_cov)])
control_mat_cov$any_mat[control_mat_cov$any_mat>0]<-1
control_mat_cov<-control_mat_cov[,c("person_id", "any_mat")]

case_preg<-merge(case_preg, case_mat_cov, by="person_id")
control_preg<-merge(control_preg, control_mat_cov, by="person_id")

# pull in the covid comorbidity

case_cov_comorb<-fread(paste0(output_cov_cases, "cases.csv"))
case_cov_comorb$any_cov<-rowSums(case_cov_comorb[,4:ncol(case_cov_comorb)])
case_cov_comorb$any_cov[case_cov_comorb$any_cov>0]<-1
case_cov_comorb<-case_cov_comorb[,c("person_id","any_cov")]

control_cov_comorb<-fread(paste0(output_cov_pregnant_control, "pregnant_cov_neg_control.csv"))
control_cov_comorb$any_cov<-rowSums(control_cov_comorb[,4:ncol(control_cov_comorb)])
control_cov_comorb$any_cov[control_cov_comorb$any_cov>0]<-1
control_cov_comorb<-control_cov_comorb[,c("person_id","any_cov")]

case_preg<-merge(case_preg, case_cov_comorb, by="person_id")
control_preg<-merge(control_preg, control_cov_comorb, by="person_id")

case_preg$duration<-case_preg$pregnancy_end_date-case_preg$pregnancy_start_date
control_preg$duration<-control_preg$pregnancy_end_date-control_preg$pregnancy_start_date

# need to give SA and SB binomial indicator
case_preg$SA<-0
case_preg$SA[case_preg$type_of_pregnancy_end=="SA"]<-1
control_preg$SA<-0
control_preg$SA[control_preg$type_of_pregnancy_end=="SA"]<-1

case_preg$SB<-0
case_preg$SB[case_preg$type_of_pregnancy_end=="SB"]<-1

control_preg$SB<-0
control_preg$SB[control_preg$type_of_pregnancy_end=="SB"]<-1

#####################################################3
# indicate case/control

case_preg$cohort<-1
control_preg$cohort<-0

##################################

model_data<-rbind(case_preg, control_preg)

model_data_severe<-rbind(case_preg[case_preg$severity==1,], control_preg)

model_data_nonsevere<-rbind(case_preg[case_preg$severity==0,], control_preg)

########################################################
#modeling

all_case_cox<-coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=model_data) 


t1_model_data<-(model_data[model_data$cov_trimester==1,])
T1_all_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t1_model_data))

t2_model_data<-(model_data[model_data$cov_trimester==2,])
T2_all_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t2_model_data))

t3_model_data<-(model_data[model_data$cov_trimester==3,])
T3_all_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t3_model_data))

######################################################################
# rest of table

SA_output<-as.data.frame(matrix(ncol=5, nrow=12))

colnames(SA_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# SA trim1", "# preg trim1", "% trim1","Relative Risk 95%CI", 
           "# SA trim2", "# preg trim2", "% trim2", "Relative Risk 95%CI",
           "# SA trim3", "# preg trim3", "% trim3" ,"Relative Risk 95%CI")

SA_output$names<-my_rows


trim1<-list()

trim1[[1]]<-sum(T1_case_mat_outcome$Spont_Abort)
trim1[[2]]<-length(T1_case_mat_outcome$Spont_Abort)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SA_output[1:3,2]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_S_case_mat_outcome$Spont_Abort)
trim1[[2]]<-length(T1_S_case_mat_outcome$Spont_Abort)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SA_output[1:3,3]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_NS_case_mat_outcome$Spont_Abort)
trim1[[2]]<-length(T1_NS_case_mat_outcome$Spont_Abort)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SA_output[1:3,4]<-unlist(trim1)



trim2<-list()

trim2[[1]]<-sum(T2_case_mat_outcome$Spont_Abort)
trim2[[2]]<-length(T2_case_mat_outcome$Spont_Abort)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SA_output[5:7,2]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_S_case_mat_outcome$Spont_Abort)
trim2[[2]]<-length(T2_S_case_mat_outcome$Spont_Abort)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SA_output[5:7,3]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_NS_case_mat_outcome$Spont_Abort)
trim2[[2]]<-length(T2_NS_case_mat_outcome$Spont_Abort)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SA_output[5:7,4]<-unlist(trim2)


trim3<-list()

trim3[[1]]<-sum(T3_case_mat_outcome$Spont_Abort)
trim3[[2]]<-length(T3_case_mat_outcome$Spont_Abort)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

SA_output[9:11,2]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3_S_case_mat_outcome$Spont_Abort)
trim3[[2]]<-length(T3_S_case_mat_outcome$Spont_Abort)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

SA_output[9:11,3]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3NS_case_mat_outcome$Spont_Abort)
trim3[[2]]<-length(T3NS_case_mat_outcome$Spont_Abort)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

SA_output[9:11,4]<-unlist(trim3)


trim1<-list()

trim1[[1]]<-sum(T1_control_mat_outcome$Spont_Abort)
trim1[[2]]<-length(T1_control_mat_outcome$Spont_Abort)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SA_output[1:3,5]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_control_mat_outcome$Spont_Abort)
trim2[[2]]<-length(T2_control_mat_outcome$Spont_Abort)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SA_output[5:7,5]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_control_mat_outcome$Spont_Abort)
trim3[[2]]<-length(T3_control_mat_outcome$Spont_Abort)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

Spont_Abort_output[9:11,5]<-unlist(trim3)


