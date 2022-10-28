#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#21/9/2022


#This script pulls drug exposure window results from trimester_drug_exposure.R 
# and transforms the data into table 1 (a,b,c) format
################################################################
CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

# denominator is all covid+ pregnancies by trimester


my_atc_folders<-c(output_cov_window_atc_3, output_cov_window_atc_4, output_cov_window_atc_5)

for(j in 1:length(my_atc_folders)){
  
  my_PREG<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))
  
  denom_trim_1<-nrow(my_PREG[my_PREG$cov_trimester==1,])
  denom_trim_2<-nrow(my_PREG[my_PREG$cov_trimester==2,])
  denom_trim_3<-nrow(my_PREG[my_PREG$cov_trimester==3,])
  
my_tables<-list.files(path=my_atc_folders[j])
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-22)
print(my_names)
# denominator is all covid+ pregnancies by trimester

#1a: all covid together

prop_plus_trim_1<-vector(length=length(my_tables))
prop_plus_trim_2<-vector(length=length(my_tables))
prop_plus_trim_3<-vector(length=length(my_tables))

prop_minus_trim_1<-vector(length=length(my_tables))
prop_minus_trim_2<-vector(length=length(my_tables))
prop_minus_trim_3<-vector(length=length(my_tables))

CI_low_plus_trim_1<-vector(length=length(my_tables))
CI_low_plus_trim_2<-vector(length=length(my_tables))
CI_low_plus_trim_3<-vector(length=length(my_tables))

CI_hi_plus_trim_1<-vector(length=length(my_tables))
CI_hi_plus_trim_2<-vector(length=length(my_tables))
CI_hi_plus_trim_3<-vector(length=length(my_tables))

CI_low_minus_trim_1<-vector(length=length(my_tables))
CI_low_minus_trim_2<-vector(length=length(my_tables))
CI_low_minus_trim_3<-vector(length=length(my_tables))

CI_hi_minus_trim_1<-vector(length=length(my_tables))
CI_hi_minus_trim_2<-vector(length=length(my_tables))
CI_hi_minus_trim_3<-vector(length=length(my_tables))

final_plus_trim_1<-vector(length=length(my_tables))
final_plus_trim_2<-vector(length=length(my_tables))
final_plus_trim_3<-vector(length=length(my_tables))

final_minus_trim_1<-vector(length=length(my_tables))
final_minus_trim_2<-vector(length=length(my_tables))
final_minus_trim_3<-vector(length=length(my_tables))

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(my_atc_folders[j],my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1_all>0){
    num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
    ci_min_1<-(prop.test(num_minus_1, denom_trim_1_all, alternative = "two.sided", correct=F))
    prop_minus_trim_1[i]<-(round(ci_min_1$estimate,3))*100
    CI_low_minus_trim_1[i]<-(round(ci_min_1$conf.int[1],3))*100
    CI_hi_minus_trim_1[i]<-(round(ci_min_1$conf.int[2],3))*100
    final_minus_trim_1[i]<-paste0(prop_minus_trim_1[i], " (", CI_low_minus_trim_1[i],"-",CI_hi_minus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")}
  
  
  # TRIMESTER 2
  if(denom_trim_2_all>0){
    num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
    ci_min_2<-(prop.test(num_minus_2, denom_trim_2_all, alternative = "two.sided", correct=F))
    prop_minus_trim_2[i]<-(round(ci_min_2$estimate,3))*100
    CI_low_minus_trim_2[i]<-(round(ci_min_2$conf.int[1],3))*100
    CI_hi_minus_trim_2[i]<-(round(ci_min_2$conf.int[2],3))*100
    final_minus_trim_2[i]<-paste0(prop_minus_trim_2[i], " (", CI_low_minus_trim_2[i],"-",CI_hi_minus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3_all>0){
    num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
    ci_min_3<-(prop.test(num_minus_3, denom_trim_3_all, alternative = "two.sided", correct=F))
    prop_minus_trim_3[i]<-(round(ci_min_3$estimate,3))*100
    CI_low_minus_trim_3[i]<-(round(ci_min_3$conf.int[1],3))*100
    CI_hi_minus_trim_3[i]<-(round(ci_min_3$conf.int[2],3))*100
    final_minus_trim_3[i]<-paste0(prop_minus_trim_3[i], " (", CI_low_minus_trim_3[i],"-",CI_hi_minus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1_all>0){
    num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
    ci_plus_1<-(prop.test(num_plus_1, denom_trim_1_all, alternative = "two.sided", correct=F))
    prop_plus_trim_1[i]<-(round(ci_plus_1$estimate,3))*100
    CI_low_plus_trim_1[i]<-(round(ci_plus_1$conf.int[1],3))*100
    CI_hi_plus_trim_1[i]<-(round(ci_plus_1$conf.int[2],3))*100
    final_plus_trim_1[i]<-paste0(prop_plus_trim_1[i], " (", CI_low_plus_trim_1[i],"-",CI_hi_plus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")
    prop_plus_trim_1[i]<-NA
    CI_low_plus_trim_1[i]<-NA
    CI_hi_plus_trim_1[i]<-NA
    final_plus_trim_1[i]<-NA}
  
  # TRIMESTER 2
  if(denom_trim_2_all>0){
    num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
    ci_plus_2<-(prop.test(num_plus_2, denom_trim_2_all, alternative = "two.sided", correct=F))
    prop_plus_trim_2[i]<-(round(ci_plus_2$estimate,3))*100
    CI_low_plus_trim_2[i]<-(round(ci_plus_2$conf.int[1],3))*100
    CI_hi_plus_trim_2[i]<-(round(ci_plus_2$conf.int[2],3))*100
    final_plus_trim_2[i]<-paste0(prop_plus_trim_2[i], " (", CI_low_plus_trim_2[i],"-",CI_hi_plus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3_all>0){
    num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
    ci_plus_3<-(prop.test(num_plus_3, denom_trim_3_all, alternative = "two.sided", correct=F))
    prop_plus_trim_3[i]<-(round(ci_plus_3$estimate,3))*100
    CI_low_plus_trim_3[i]<-(round(ci_plus_3$conf.int[1],3))*100
    CI_hi_plus_trim_3[i]<-(round(ci_plus_3$conf.int[2],3))*100
    final_plus_trim_3[i]<-paste0(prop_plus_trim_3[i], " (", CI_low_plus_trim_3[i],"-",CI_hi_plus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
}

table_1a<-as.data.frame(cbind((my_names), (final_minus_trim_1),(final_plus_trim_1), 
                              (final_minus_trim_2),(final_plus_trim_2), (final_minus_trim_3), (final_plus_trim_3)))

colnames(table_1a)<-c("Drug", "trim 1, -30 days", "trim 1, +30 days", "trim 2, -30 days", 
                      "trim 2, +30 days","trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1a, paste0(final_output_dir,"supplement/",DAP,"ATC",j+2, "_table_1_a.csv"))


###############################################################################
#1b" non-severe


my_PREG_NS<-my_PREG[my_PREG$severity==0,]
denom_trim_1_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==1,])
denom_trim_2_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==2,])
denom_trim_3_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==3,])

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(my_atc_folders[j],my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  my_data<-my_data[my_data$severity=="0"]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1_NS>0){
    num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
    ci_min_1<-(prop.test(num_minus_1, denom_trim_1_NS, alternative = "two.sided", correct=F))
    prop_minus_trim_1[i]<-(round(ci_min_1$estimate,3))*100
    CI_low_minus_trim_1[i]<-(round(ci_min_1$conf.int[1],3))*100
    CI_hi_minus_trim_1[i]<-(round(ci_min_1$conf.int[2],3))*100
    final_minus_trim_1[i]<-paste0(prop_minus_trim_1[i], " (", CI_low_minus_trim_1[i],"-",CI_hi_minus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")}
  
  
  # TRIMESTER 2
  if(denom_trim_2_NS>0){
    num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
    ci_min_2<-(prop.test(num_minus_2, denom_trim_2_NS, alternative = "two.sided", correct=F))
    prop_minus_trim_2[i]<-(round(ci_min_2$estimate,3))*100
    CI_low_minus_trim_2[i]<-(round(ci_min_2$conf.int[1],3))*100
    CI_hi_minus_trim_2[i]<-(round(ci_min_2$conf.int[2],3))*100
    final_minus_trim_2[i]<-paste0(prop_minus_trim_2[i], " (", CI_low_minus_trim_2[i],"-",CI_hi_minus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3_NS>0){
    num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
    ci_min_3<-(prop.test(num_minus_3, denom_trim_3_NS, alternative = "two.sided", correct=F))
    prop_minus_trim_3[i]<-(round(ci_min_3$estimate,3))*100
    CI_low_minus_trim_3[i]<-(round(ci_min_3$conf.int[1],3))*100
    CI_hi_minus_trim_3[i]<-(round(ci_min_3$conf.int[2],3))*100
    final_minus_trim_3[i]<-paste0(prop_minus_trim_3[i], " (", CI_low_minus_trim_3[i],"-",CI_hi_minus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1_NS>0){
    num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
    ci_plus_1<-(prop.test(num_plus_1, denom_trim_1_NS, alternative = "two.sided", correct=F))
    prop_plus_trim_1[i]<-(round(ci_plus_1$estimate,3))*100
    CI_low_plus_trim_1[i]<-(round(ci_plus_1$conf.int[1],3))*100
    CI_hi_plus_trim_1[i]<-(round(ci_plus_1$conf.int[2],3))*100
    final_plus_trim_1[i]<-paste0(prop_plus_trim_1[i], " (", CI_low_plus_trim_1[i],"-",CI_hi_plus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")
    prop_plus_trim_1[i]<-NA
    CI_low_plus_trim_1[i]<-NA
    CI_hi_plus_trim_1[i]<-NA
    final_plus_trim_1[i]<-NA}
  
  # TRIMESTER 2
  if(denom_trim_2_NS>0){
    num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
    ci_plus_2<-(prop.test(num_plus_2, denom_trim_2_NS, alternative = "two.sided", correct=F))
    prop_plus_trim_2[i]<-(round(ci_plus_2$estimate,3))*100
    CI_low_plus_trim_2[i]<-(round(ci_plus_2$conf.int[1],3))*100
    CI_hi_plus_trim_2[i]<-(round(ci_plus_2$conf.int[2],3))*100
    final_plus_trim_2[i]<-paste0(prop_plus_trim_2[i], " (", CI_low_plus_trim_2[i],"-",CI_hi_plus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3_NS>0){
    num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
    ci_plus_3<-(prop.test(num_plus_3, denom_trim_3_NS, alternative = "two.sided", correct=F))
    prop_plus_trim_3[i]<-(round(ci_plus_3$estimate,3))*100
    CI_low_plus_trim_3[i]<-(round(ci_plus_3$conf.int[1],3))*100
    CI_hi_plus_trim_3[i]<-(round(ci_plus_3$conf.int[2],3))*100
    final_plus_trim_3[i]<-paste0(prop_plus_trim_3[i], " (", CI_low_plus_trim_3[i],"-",CI_hi_plus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
}

table_1b<-as.data.frame(cbind((my_names), (final_minus_trim_1),(final_plus_trim_1), 
                              (final_minus_trim_2),(final_plus_trim_2), (final_minus_trim_3), (final_plus_trim_3)))

colnames(table_1b)<-c("Drug", "trim 1, -30 days", "trim 1, +30 days", "trim 2, -30 days", 
                      "trim 2, +30 days","trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1b, paste0(final_output_dir,"supplement/", DAP,"ATC",j+2, "_table_1_b.csv"))

#############################################################################################
#1c: severe

my_PREG_S<-my_PREG[my_PREG$severity==1,]
denom_trim_1_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==1,])
denom_trim_2_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==2,])
denom_trim_3_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==3,])

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(my_atc_folders[j],my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  my_data<-my_data[my_data$severity=="1"]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1_S>0){
    num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
    ci_min_1<-(prop.test(num_minus_1, denom_trim_1_S, alternative = "two.sided", correct=F))
    prop_minus_trim_1[i]<-(round(ci_min_1$estimate,3))*100
    CI_low_minus_trim_1[i]<-(round(ci_min_1$conf.int[1],3))*100
    CI_hi_minus_trim_1[i]<-(round(ci_min_1$conf.int[2],3))*100
    final_minus_trim_1[i]<-paste0(prop_minus_trim_1[i], " (", CI_low_minus_trim_1[i],"-",CI_hi_minus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")}
  
  
  # TRIMESTER 2
  if(denom_trim_2_S>0){
    num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
    ci_min_2<-(prop.test(num_minus_2, denom_trim_2_S, alternative = "two.sided", correct=F))
    prop_minus_trim_2[i]<-(round(ci_min_2$estimate,3))*100
    CI_low_minus_trim_2[i]<-(round(ci_min_2$conf.int[1],3))*100
    CI_hi_minus_trim_2[i]<-(round(ci_min_2$conf.int[2],3))*100
    final_minus_trim_2[i]<-paste0(prop_minus_trim_2[i], " (", CI_low_minus_trim_2[i],"-",CI_hi_minus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3_S>0){
    num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
    ci_min_3<-(prop.test(num_minus_3, denom_trim_3_S, alternative = "two.sided", correct=F))
    prop_minus_trim_3[i]<-(round(ci_min_3$estimate,3))*100
    CI_low_minus_trim_3[i]<-(round(ci_min_3$conf.int[1],3))*100
    CI_hi_minus_trim_3[i]<-(round(ci_min_3$conf.int[2],3))*100
    final_minus_trim_3[i]<-paste0(prop_minus_trim_3[i], " (", CI_low_minus_trim_3[i],"-",CI_hi_minus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1_S>0){
    num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
    ci_plus_1<-(prop.test(num_plus_1, denom_trim_1_S, alternative = "two.sided", correct=F))
    prop_plus_trim_1[i]<-(round(ci_plus_1$estimate,3))*100
    CI_low_plus_trim_1[i]<-(round(ci_plus_1$conf.int[1],3))*100
    CI_hi_plus_trim_1[i]<-(round(ci_plus_1$conf.int[2],3))*100
    final_plus_trim_1[i]<-paste0(prop_plus_trim_1[i], " (", CI_low_plus_trim_1[i],"-",CI_hi_plus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")
    prop_plus_trim_1[i]<-NA
    CI_low_plus_trim_1[i]<-NA
    CI_hi_plus_trim_1[i]<-NA
    final_plus_trim_1[i]<-NA}
  
  # TRIMESTER 2
  if(denom_trim_2_S>0){
    num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
    ci_plus_2<-(prop.test(num_plus_2, denom_trim_2_S, alternative = "two.sided", correct=F))
    prop_plus_trim_2[i]<-(round(ci_plus_2$estimate,3))*100
    CI_low_plus_trim_2[i]<-(round(ci_plus_2$conf.int[1],3))*100
    CI_hi_plus_trim_2[i]<-(round(ci_plus_2$conf.int[2],3))*100
    final_plus_trim_2[i]<-paste0(prop_plus_trim_2[i], " (", CI_low_plus_trim_2[i],"-",CI_hi_plus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3_S>0){
    num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
    ci_plus_3<-(prop.test(num_plus_3, denom_trim_3_S, alternative = "two.sided", correct=F))
    prop_plus_trim_3[i]<-(round(ci_plus_3$estimate,3))*100
    CI_low_plus_trim_3[i]<-(round(ci_plus_3$conf.int[1],3))*100
    CI_hi_plus_trim_3[i]<-(round(ci_plus_3$conf.int[2],3))*100
    final_plus_trim_3[i]<-paste0(prop_plus_trim_3[i], " (", CI_low_plus_trim_3[i],"-",CI_hi_plus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
}

table_1c<-as.data.frame(cbind((my_names), (final_minus_trim_1),(final_plus_trim_1), 
                              (final_minus_trim_2),(final_plus_trim_2), (final_minus_trim_3), (final_plus_trim_3)))

colnames(table_1c)<-c("Drug", "trim 1, -30 days", "trim 1, +30 days", "trim 2, -30 days", 
                      "trim 2, +30 days","trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1c, paste0(final_output_dir,"supplement/", DAP,"ATC",j+2,"_table_1_c.csv"))

print("final outputs with proportions and 95% confidence intervals are stored in g_output/final")
print("warnings are generated when count==0, ignore them, it's OK")


}
