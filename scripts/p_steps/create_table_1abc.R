#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#21/9/2022


#This script pulls drug exposure window results from trimester_drug_exposure.R 
# and transforms the data into table 1 (a,b,c) format
################################################################
CDM_source<-fread(paste0(path_CDM,"CDM_source.csv"))
DAP<-CDM_source$data_access_provider_name

my_tables<-list.files(path=output_cov_window_atc_2)
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-22)

# denominator is all covid+ pregnancies by trimester

my_PREG<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))

denom_trim_1<-nrow(my_PREG[my_PREG$cov_trimester==1,])
denom_trim_2<-nrow(my_PREG[my_PREG$cov_trimester==2,])
denom_trim_3<-nrow(my_PREG[my_PREG$cov_trimester==3,])


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
  my_data<-fread(paste0(output_cov_window_atc_2,my_tables[i]))
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1>0){
  num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
  ci_min_1<-(prop.test(num_minus_1, denom_trim_1, alternative = "two.sided", correct=F))
  prop_minus_trim_1[i]<-round(ci_min_1$estimate,2)
  CI_low_minus_trim_1[i]<-round(ci_min_1$conf.int[1],2)
  CI_hi_minus_trim_1[i]<-round(ci_min_1$conf.int[2],2)
  final_minus_trim_1[i]<-paste0(prop_minus_trim_1[i], " (", CI_low_minus_trim_1[i],"-",CI_hi_minus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")}
  
  
  # TRIMESTER 2
  if(denom_trim_2>0){
  num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
  ci_min_2<-(prop.test(num_minus_2, denom_trim_2, alternative = "two.sided", correct=F))
  prop_minus_trim_2[i]<-round(ci_min_2$estimate,2)
  CI_low_minus_trim_2[i]<-round(ci_min_2$conf.int[1],2)
  CI_hi_minus_trim_2[i]<-round(ci_min_2$conf.int[2],2)
  final_minus_trim_2[i]<-paste0(prop_minus_trim_2[i], " (", CI_low_minus_trim_2[i],"-",CI_hi_minus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3>0){
  num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
  ci_min_3<-(prop.test(num_minus_3, denom_trim_3, alternative = "two.sided", correct=F))
  prop_minus_trim_3[i]<-round(ci_min_3$estimate,2)
  CI_low_minus_trim_3[i]<-round(ci_min_3$conf.int[1],2)
  CI_hi_minus_trim_3[i]<-round(ci_min_3$conf.int[2],2)
  final_minus_trim_3[i]<-paste0(prop_minus_trim_3[i], " (", CI_low_minus_trim_3[i],"-",CI_hi_minus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1>0){
  num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
  ci_plus_1<-(prop.test(num_plus_1, denom_trim_1, alternative = "two.sided", correct=F))
  prop_plus_trim_1[i]<-round(ci_plus_1$estimate,2)
  CI_low_plus_trim_1[i]<-round(ci_plus_1$conf.int[1],2)
  CI_hi_plus_trim_1[i]<-round(ci_plus_1$conf.int[2],2)
  final_plus_trim_1[i]<-paste0(prop_plus_trim_1[i], " (", CI_low_plus_trim_1[i],"-",CI_hi_plus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")
    prop_plus_trim_1[i]<-NA
    CI_low_plus_trim_1[i]<-NA
    CI_hi_plus_trim_1[i]<-NA
    final_plus_trim_1[i]<-NA}

  # TRIMESTER 2
  if(denom_trim_2>0){
  num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
  ci_plus_2<-(prop.test(num_plus_2, denom_trim_2, alternative = "two.sided", correct=F))
  prop_plus_trim_2[i]<-round(ci_plus_2$estimate,2)
  CI_low_plus_trim_2[i]<-round(ci_plus_2$conf.int[1],2)
  CI_hi_plus_trim_2[i]<-round(ci_plus_2$conf.int[2],2)
  final_plus_trim_2[i]<-paste0(prop_plus_trim_2[i], " (", CI_low_plus_trim_2[i],"-",CI_hi_plus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3>0){
  num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
  ci_plus_3<-(prop.test(num_plus_3, denom_trim_3, alternative = "two.sided", correct=F))
  prop_plus_trim_3[i]<-round(ci_plus_3$estimate,2)
  CI_low_plus_trim_3[i]<-round(ci_plus_3$conf.int[1],2)
  CI_hi_plus_trim_3[i]<-round(ci_plus_3$conf.int[2],2)
  final_plus_trim_3[i]<-paste0(prop_plus_trim_3[i], " (", CI_low_plus_trim_3[i],"-",CI_hi_plus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}

}

table_1a<-as.data.frame(cbind((my_names), (final_minus_trim_1),(final_plus_trim_1), 
                        (final_minus_trim_2),(final_plus_trim_2), (final_minus_trim_3), (final_plus_trim_3)))

colnames(table_1a)<-c("Drug", "trim 1, -30 days", "trim 1, +30 days", "trim 2, -30 days", 
                      "trim 2, +30 days","trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1a, paste0(final_output_dir,DAP, "_table_1_a.csv"))


###############################################################################
#1b" non-severe


my_PREG_NS<-my_PREG[my_PREG$severity==0,]
denom_trim_1<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==1,])
denom_trim_2<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==2,])
denom_trim_3<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==3,])

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_cov_window_atc_2,my_tables[i]))
  my_data<-my_data[my_data$severity=="0"]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1>0){
    num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
    ci_min_1<-(prop.test(num_minus_1, denom_trim_1, alternative = "two.sided", correct=F))
    prop_minus_trim_1[i]<-round(ci_min_1$estimate,2)
    CI_low_minus_trim_1[i]<-round(ci_min_1$conf.int[1],2)
    CI_hi_minus_trim_1[i]<-round(ci_min_1$conf.int[2],2)
    final_minus_trim_1[i]<-paste0(prop_minus_trim_1[i], " (", CI_low_minus_trim_1[i],"-",CI_hi_minus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")}
  
  
  # TRIMESTER 2
  if(denom_trim_2>0){
    num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
    ci_min_2<-(prop.test(num_minus_2, denom_trim_2, alternative = "two.sided", correct=F))
    prop_minus_trim_2[i]<-round(ci_min_2$estimate,2)
    CI_low_minus_trim_2[i]<-round(ci_min_2$conf.int[1],2)
    CI_hi_minus_trim_2[i]<-round(ci_min_2$conf.int[2],2)
    final_minus_trim_2[i]<-paste0(prop_minus_trim_2[i], " (", CI_low_minus_trim_2[i],"-",CI_hi_minus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3>0){
    num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
    ci_min_3<-(prop.test(num_minus_3, denom_trim_3, alternative = "two.sided", correct=F))
    prop_minus_trim_3[i]<-round(ci_min_3$estimate,2)
    CI_low_minus_trim_3[i]<-round(ci_min_3$conf.int[1],2)
    CI_hi_minus_trim_3[i]<-round(ci_min_3$conf.int[2],2)
    final_minus_trim_3[i]<-paste0(prop_minus_trim_3[i], " (", CI_low_minus_trim_3[i],"-",CI_hi_minus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1>0){
    num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
    ci_plus_1<-(prop.test(num_plus_1, denom_trim_1, alternative = "two.sided", correct=F))
    prop_plus_trim_1[i]<-round(ci_plus_1$estimate,2)
    CI_low_plus_trim_1[i]<-round(ci_plus_1$conf.int[1],2)
    CI_hi_plus_trim_1[i]<-round(ci_plus_1$conf.int[2],2)
    final_plus_trim_1[i]<-paste0(prop_plus_trim_1[i], " (", CI_low_plus_trim_1[i],"-",CI_hi_plus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")
    prop_plus_trim_1[i]<-NA
    CI_low_plus_trim_1[i]<-NA
    CI_hi_plus_trim_1[i]<-NA
    final_plus_trim_1[i]<-NA}
  
  # TRIMESTER 2
  if(denom_trim_2>0){
    num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
    ci_plus_2<-(prop.test(num_plus_2, denom_trim_2, alternative = "two.sided", correct=F))
    prop_plus_trim_2[i]<-round(ci_plus_2$estimate,2)
    CI_low_plus_trim_2[i]<-round(ci_plus_2$conf.int[1],2)
    CI_hi_plus_trim_2[i]<-round(ci_plus_2$conf.int[2],2)
    final_plus_trim_2[i]<-paste0(prop_plus_trim_2[i], " (", CI_low_plus_trim_2[i],"-",CI_hi_plus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3>0){
    num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
    ci_plus_3<-(prop.test(num_plus_3, denom_trim_3, alternative = "two.sided", correct=F))
    prop_plus_trim_3[i]<-round(ci_plus_3$estimate,2)
    CI_low_plus_trim_3[i]<-round(ci_plus_3$conf.int[1],2)
    CI_hi_plus_trim_3[i]<-round(ci_plus_3$conf.int[2],2)
    final_plus_trim_3[i]<-paste0(prop_plus_trim_3[i], " (", CI_low_plus_trim_3[i],"-",CI_hi_plus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
}

table_1b<-as.data.frame(cbind((my_names), (final_minus_trim_1),(final_plus_trim_1), 
                              (final_minus_trim_2),(final_plus_trim_2), (final_minus_trim_3), (final_plus_trim_3)))

colnames(table_1b)<-c("Drug", "trim 1, -30 days", "trim 1, +30 days", "trim 2, -30 days", 
                      "trim 2, +30 days","trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1b, paste0(final_output_dir, DAP, "_table_1_b.csv"))

#############################################################################################
#1c: severe

my_PREG_S<-my_PREG[my_PREG$severity==1,]
denom_trim_1<-nrow(my_PREG_S[my_PREG_S$cov_trimester==1,])
denom_trim_2<-nrow(my_PREG_S[my_PREG_S$cov_trimester==2,])
denom_trim_3<-nrow(my_PREG_S[my_PREG_S$cov_trimester==3,])

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_cov_window_atc_2,my_tables[i]))
  my_data<-my_data[my_data$severity=="0"]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1>0){
    num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
    ci_min_1<-(prop.test(num_minus_1, denom_trim_1, alternative = "two.sided", correct=F))
    prop_minus_trim_1[i]<-round(ci_min_1$estimate,2)
    CI_low_minus_trim_1[i]<-round(ci_min_1$conf.int[1],2)
    CI_hi_minus_trim_1[i]<-round(ci_min_1$conf.int[2],2)
    final_minus_trim_1[i]<-paste0(prop_minus_trim_1[i], " (", CI_low_minus_trim_1[i],"-",CI_hi_minus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")}
  
  
  # TRIMESTER 2
  if(denom_trim_2>0){
    num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
    ci_min_2<-(prop.test(num_minus_2, denom_trim_2, alternative = "two.sided", correct=F))
    prop_minus_trim_2[i]<-round(ci_min_2$estimate,2)
    CI_low_minus_trim_2[i]<-round(ci_min_2$conf.int[1],2)
    CI_hi_minus_trim_2[i]<-round(ci_min_2$conf.int[2],2)
    final_minus_trim_2[i]<-paste0(prop_minus_trim_2[i], " (", CI_low_minus_trim_2[i],"-",CI_hi_minus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3>0){
    num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
    ci_min_3<-(prop.test(num_minus_3, denom_trim_3, alternative = "two.sided", correct=F))
    prop_minus_trim_3[i]<-round(ci_min_3$estimate,2)
    CI_low_minus_trim_3[i]<-round(ci_min_3$conf.int[1],2)
    CI_hi_minus_trim_3[i]<-round(ci_min_3$conf.int[2],2)
    final_minus_trim_3[i]<-paste0(prop_minus_trim_3[i], " (", CI_low_minus_trim_3[i],"-",CI_hi_minus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1>0){
    num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
    ci_plus_1<-(prop.test(num_plus_1, denom_trim_1, alternative = "two.sided", correct=F))
    prop_plus_trim_1[i]<-round(ci_plus_1$estimate,2)
    CI_low_plus_trim_1[i]<-round(ci_plus_1$conf.int[1],2)
    CI_hi_plus_trim_1[i]<-round(ci_plus_1$conf.int[2],2)
    final_plus_trim_1[i]<-paste0(prop_plus_trim_1[i], " (", CI_low_plus_trim_1[i],"-",CI_hi_plus_trim_1[i],")" )
  }else{print("no covid infections in trimester 1, denominator=0")
    prop_plus_trim_1[i]<-NA
    CI_low_plus_trim_1[i]<-NA
    CI_hi_plus_trim_1[i]<-NA
    final_plus_trim_1[i]<-NA}
  
  # TRIMESTER 2
  if(denom_trim_2>0){
    num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
    ci_plus_2<-(prop.test(num_plus_2, denom_trim_2, alternative = "two.sided", correct=F))
    prop_plus_trim_2[i]<-round(ci_plus_2$estimate,2)
    CI_low_plus_trim_2[i]<-round(ci_plus_2$conf.int[1],2)
    CI_hi_plus_trim_2[i]<-round(ci_plus_2$conf.int[2],2)
    final_plus_trim_2[i]<-paste0(prop_plus_trim_2[i], " (", CI_low_plus_trim_2[i],"-",CI_hi_plus_trim_2[i],")" )
  }else{print("no covid infections in trimester 2, denominator=0")}
  
  # TRIMESTER 3
  if(denom_trim_3>0){
    num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
    ci_plus_3<-(prop.test(num_plus_3, denom_trim_3, alternative = "two.sided", correct=F))
    prop_plus_trim_3[i]<-round(ci_plus_3$estimate,2)
    CI_low_plus_trim_3[i]<-round(ci_plus_3$conf.int[1],2)
    CI_hi_plus_trim_3[i]<-round(ci_plus_3$conf.int[2],2)
    final_plus_trim_3[i]<-paste0(prop_plus_trim_3[i], " (", CI_low_plus_trim_3[i],"-",CI_hi_plus_trim_3[i],")" )
  }else{print("no covid infections in trimester 3, denominator=0")}
  
}

table_1c<-as.data.frame(cbind((my_names), (final_minus_trim_1),(final_plus_trim_1), 
                              (final_minus_trim_2),(final_plus_trim_2), (final_minus_trim_3), (final_plus_trim_3)))

colnames(table_1c)<-c("Drug", "trim 1, -30 days", "trim 1, +30 days", "trim 2, -30 days", 
                      "trim 2, +30 days","trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1c, paste0(final_output_dir, DAP,"_table_1_c.csv"))

print("final outputs with proportions and 95% confidence intervals are stored in g_output/final")
print("warnings are generated when count==0, ignore them, it's OK")

