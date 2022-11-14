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

# restrict to pregnancies that reach at least 22 weeks? 

model_data_severe_cases<-case_preg[case_preg$severity==1,]
model_data_severe<-as.data.frame(rbind(model_data_severe_cases, control_preg))

model_data_nonsevere_cases<-case_preg[case_preg$severity==0,]
model_data_nonsevere<-as.data.frame(rbind(model_data_nonsevere_cases, control_preg))




########################################################
#modeling
########################################################



all_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=model_data))

# all cases

all_cox_SA_results<-list()

t1_model_data<-(model_data[model_data$cov_trimester==1,])
T1_all_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t1_model_data))
T1_all_confint<-round(T1_all_case_cox$conf.int, 2)
all_cox_SA_results[[1]]<-paste0(T1_all_confint[1,1], " (", T1_all_confint[1,3], "-", T1_all_confint[1,4], ")")

t2_model_data<-(model_data[model_data$cov_trimester==2,])
T2_all_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t2_model_data))
T2_all_confint<-round(T2_all_case_cox$conf.int, 2)
all_cox_SA_results[[2]]<-paste0(T2_all_confint[1,1], " (", T2_all_confint[1,3], "-", T2_all_confint[1,4], ")")


t3_model_data<-(model_data[model_data$cov_trimester==3,])
T3_all_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t3_model_data))
T3_all_confint<-round(T3_all_case_cox$conf.int, 2)
all_cox_SA_results[[3]]<-paste0(T3_all_confint[1,1], " (", T3_all_confint[1,3], "-", T3_all_confint[1,4], ")")


##############################SEVERE
severe_cox_SA_results<-list()

t1_model_data_severe<-(model_data_severe[model_data_severe$cov_trimester==1,])
T1_severe_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t1_model_data_severe))
T1_severe_confint<-round(T1_severe_case_cox$conf.int, 2)
severe_cox_SA_results[[1]]<-paste0(T1_severe_confint[1,1], " (", T1_severe_confint[1,3], "-", T1_severe_confint[1,4], ")")


t2_model_data_severe<-(model_data_severe[model_data_severe$cov_trimester==2,])
T2_severe_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t2_model_data_severe))
T2_severe_confint<-round(T2_severe_case_cox$conf.int, 2)
severe_cox_SA_results[[2]]<-paste0(T2_severe_confint[1,1], " (", T2_severe_confint[1,3], "-", T2_severe_confint[1,4], ")")


t3_model_data_severe<-(model_data_severe[model_data_severe$cov_trimester==3,])
T3_severe_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t3_model_data_severe))
T3_severe_confint<-round(T3_severe_case_cox$conf.int, 2)
severe_cox_SA_results[[3]]<-paste0(T3_severe_confint[1,1], " (", T3_severe_confint[1,3], "-", T3_severe_confint[1,4], ")")

################################NONSEVERE

nonsevere_cox_SA_results<-list()

t1_model_data_nonsevere<-(model_data_nonsevere[model_data_nonsevere$cov_trimester==1,])
T1_nonsevere_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t1_model_data_nonsevere))
T1_nonsevere_confint<-round(T1_nonsevere_case_cox$conf.int, 2)
nonsevere_cox_SA_results[[1]]<-paste0(T1_nonsevere_confint[1,1], " (", T1_nonsevere_confint[1,3], "-", T1_nonsevere_confint[1,4], ")")


t2_model_data_nonsevere<-(model_data_nonsevere[model_data_nonsevere$cov_trimester==2,])
T2_nonsevere_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t2_model_data_nonsevere))
T2_nonsevere_confint<-round(T2_nonsevere_case_cox$conf.int, 2)
nonsevere_cox_SA_results[[1]]<-paste0(T2_nonsevere_confint[1,1], " (", T2_nonsevere_confint[1,3], "-", T2_nonsevere_confint[1,4], ")")


t3_model_data_nonsevere<-(model_data_nonsevere[model_data_nonsevere$cov_trimester==3,])
T3_nonsevere_case_cox<-summary(coxph(Surv(duration, SA) ~ cohort+any_cov+any_mat , data=t3_model_data_nonsevere))
T3_nonsevere_confint<-round(T3_nonsevere_case_cox$conf.int, 2)
nonsevere_cox_SA_results[[1]]<-paste0(T3_nonsevere_confint[1,1], " (", T3_nonsevere_confint[1,3], "-", T3_nonsevere_confint[1,4], ")")



######################################################################
# rest of table

SA_output<-as.data.frame(matrix(ncol=5, nrow=12))

colnames(SA_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# SA trim1", "# preg trim1", "% trim1","Relative Risk 95%CI", 
           "# SA trim2", "# preg trim2", "% trim2", "Relative Risk 95%CI",
           "# SA trim3", "# preg trim3", "% trim3" ,"Relative Risk 95%CI")

SA_output$names<-my_rows

SA_output[c(4, 8, 12), 2]<-unlist(all_cox_SA_results)
SA_output[c(4, 8, 12), 3]<-unlist(severe_cox_SA_results)
SA_output[c(4, 8, 12), 4]<-unlist(nonsevere_cox_SA_results)


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

trim2[[1]]<-sum(T2_SA_case_mat_outcome$Spont_Abort)
trim2[[2]]<-length(T2_SA_case_mat_outcome$Spont_Abort)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SA_output[5:7,2]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_SA_S_case_mat_outcome$Spont_Abort)
trim2[[2]]<-length(T2_SA_S_case_mat_outcome$Spont_Abort)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SA_output[5:7,3]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_SA_NS_case_mat_outcome$Spont_Abort)
trim2[[2]]<-length(T2_SA_NS_case_mat_outcome$Spont_Abort)
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

trim3[[1]]<-sum(T3_NS_case_mat_outcome$Spont_Abort)
trim3[[2]]<-length(T3_NS_case_mat_outcome$Spont_Abort)
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

SA_output[9:11,5]<-unlist(trim3)

fwrite(SA_output, paste0(final_output_dir, "table_5_Spont_Abort.csv"))


##############################################################
# repeat for still birth
#############################################################


########################################################
#modeling

all_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=model_data))

# all cases

all_cox_SB_results<-list()

t1_model_data<-(model_data[model_data$cov_trimester==1,])
T1_all_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t1_model_data))
T1_all_confint<-round(T1_all_case_cox$conf.int, 2)
all_cox_SB_results[[1]]<-paste0(T1_all_confint[1,1], " (", T1_all_confint[1,3], "-", T1_all_confint[1,4], ")")

t2_model_data<-(model_data[model_data$cov_trimester==2,])
T2_all_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t2_model_data))
T2_all_confint<-round(T2_all_case_cox$conf.int, 2)
all_cox_SB_results[[2]]<-paste0(T2_all_confint[1,1], " (", T2_all_confint[1,3], "-", T2_all_confint[1,4], ")")


t3_model_data<-(model_data[model_data$cov_trimester==3,])
T3_all_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t3_model_data))
T3_all_confint<-round(T3_all_case_cox$conf.int, 2)
all_cox_SB_results[[3]]<-paste0(T3_all_confint[1,1], " (", T3_all_confint[1,3], "-", T3_all_confint[1,4], ")")


##############################SEVERE
severe_cox_SB_results<-list()

t1_model_data_severe<-(model_data_severe[model_data_severe$cov_trimester==1,])
T1_severe_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t1_model_data_severe))
T1_severe_confint<-round(T1_severe_case_cox$conf.int, 2)
severe_cox_SB_results[[1]]<-paste0(T1_severe_confint[1,1], " (", T1_severe_confint[1,3], "-", T1_severe_confint[1,4], ")")


t2_model_data_severe<-(model_data_severe[model_data_severe$cov_trimester==2,])
T2_severe_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t2_model_data_severe))
T2_severe_confint<-round(T2_severe_case_cox$conf.int, 2)
severe_cox_SB_results[[2]]<-paste0(T2_severe_confint[1,1], " (", T2_severe_confint[1,3], "-", T2_severe_confint[1,4], ")")


t3_model_data_severe<-(model_data_severe[model_data_severe$cov_trimester==3,])
T3_severe_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t3_model_data_severe))
T3_severe_confint<-round(T3_severe_case_cox$conf.int, 2)
severe_cox_SB_results[[3]]<-paste0(T3_severe_confint[1,1], " (", T3_severe_confint[1,3], "-", T3_severe_confint[1,4], ")")

################################NONSEVERE

nonsevere_cox_SB_results<-list()

t1_model_data_nonsevere<-(model_data_nonsevere[model_data_nonsevere$cov_trimester==1,])
T1_nonsevere_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t1_model_data_nonsevere))
T1_nonsevere_confint<-round(T1_nonsevere_case_cox$conf.int, 2)
nonsevere_cox_SB_results[[1]]<-paste0(T1_nonsevere_confint[1,1], " (", T1_nonsevere_confint[1,3], "-", T1_nonsevere_confint[1,4], ")")


t2_model_data_nonsevere<-(model_data_nonsevere[model_data_nonsevere$cov_trimester==2,])
T2_nonsevere_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t2_model_data_nonsevere))
T2_nonsevere_confint<-round(T2_nonsevere_case_cox$conf.int, 2)
nonsevere_cox_SB_results[[1]]<-paste0(T2_nonsevere_confint[1,1], " (", T2_nonsevere_confint[1,3], "-", T2_nonsevere_confint[1,4], ")")


t3_model_data_nonsevere<-(model_data_nonsevere[model_data_nonsevere$cov_trimester==3,])
T3_nonsevere_case_cox<-summary(coxph(Surv(duration, SB) ~ cohort+any_cov+any_mat , data=t3_model_data_nonsevere))
T3_nonsevere_confint<-round(T3_nonsevere_case_cox$conf.int, 2)
nonsevere_cox_SB_results[[1]]<-paste0(T3_nonsevere_confint[1,1], " (", T3_nonsevere_confint[1,3], "-", T3_nonsevere_confint[1,4], ")")


######################################################################
# rest of table

SB_output<-as.data.frame(matrix(ncol=5, nrow=12))

colnames(SB_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# SB trim1", "# preg trim1", "% trim1","Relative Risk 95%CI", 
           "# SB trim2", "# preg trim2", "% trim2", "Relative Risk 95%CI",
           "# SB trim3", "# preg trim3", "% trim3" ,"Relative Risk 95%CI")

SB_output$names<-my_rows

SB_output[c(4, 8, 12), 2]<-unlist(all_cox_SB_results)
SB_output[c(4, 8, 12), 3]<-unlist(severe_cox_SB_results)
SB_output[c(4, 8, 12), 4]<-unlist(nonsevere_cox_SB_results)


trim1<-list()

trim1[[1]]<-sum(T1_22_case_mat_outcome$Still_Birth)
trim1[[2]]<-length(T1_22_case_mat_outcome$Still_Birth)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SB_output[1:3,2]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_22S_case_mat_outcome$Still_Birth)
trim1[[2]]<-length(T1_22S_case_mat_outcome$Still_Birth)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SB_output[1:3,3]<-unlist(trim1)

trim1<-list()

trim1[[1]]<-sum(T1_22NS_case_mat_outcome$Still_Birth)
trim1[[2]]<-length(T1_22NS_case_mat_outcome$Still_Birth)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SB_output[1:3,4]<-unlist(trim1)



trim2<-list()

trim2[[1]]<-sum(T2_22_case_mat_outcome$Still_Birth)
trim2[[2]]<-length(T2_22_case_mat_outcome$Still_Birth)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SB_output[5:7,2]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_22S_case_mat_outcome$Still_Birth)
trim2[[2]]<-length(T2_22S_case_mat_outcome$Still_Birth)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SB_output[5:7,3]<-unlist(trim2)



trim2<-list()

trim2[[1]]<-sum(T2_22NS_case_mat_outcome$Still_Birth)
trim2[[2]]<-length(T2_22NS_case_mat_outcome$Still_Birth)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SB_output[5:7,4]<-unlist(trim2)


trim3<-list()

trim3[[1]]<-sum(T3_case_mat_outcome$Still_Birth)
trim3[[2]]<-length(T3_case_mat_outcome$Still_Birth)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

SB_output[9:11,2]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3_22S_case_mat_outcome$Still_Birth)
trim3[[2]]<-length(T3_22S_case_mat_outcome$Still_Birth)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

SB_output[9:11,3]<-unlist(trim3)

trim3<-list()

trim3[[1]]<-sum(T3_22NS_case_mat_outcome$Still_Birth)
trim3[[2]]<-length(T3_22NS_case_mat_outcome$Still_Birth)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

SB_output[9:11,4]<-unlist(trim3)


trim1<-list()

trim1[[1]]<-sum(T1_22_control_mat_outcome$Still_Birth)
trim1[[2]]<-length(T1_22_control_mat_outcome$Still_Birth)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,3)*100)," (", (round(trim1_prop$conf.int[1],3)*100),"-",(round(trim1_prop$conf.int[2],3)*100),")")

SB_output[1:3,5]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_22_control_mat_outcome$Still_Birth)
trim2[[2]]<-length(T2_22_control_mat_outcome$Still_Birth)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,3)*100)," (", (round(trim2_prop$conf.int[1],3)*100),"-",(round(trim2_prop$conf.int[2],3)*100),")")

SB_output[5:7,5]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_22_control_mat_outcome$Still_Birth)
trim3[[2]]<-length(T3_22_control_mat_outcome$Still_Birth)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,3)*100)," (", (round(trim3_prop$conf.int[1],3)*100),"-",(round(trim3_prop$conf.int[2],3)*100),")")

SB_output[9:11,5]<-unlist(trim3)

fwrite(SB_output, paste0(final_output_dir, "table_5_Still_Birth.csv"))


