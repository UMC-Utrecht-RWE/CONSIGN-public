#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#21/9/2022


#This script pulls drug exposure window results from trimester_drug_exposure.R 
# 

# CONTROLS PREGNANT DURING PANDEMIC BUT NO COVID (COVID_Date from matched CASE)

################################################################
CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

my_tables<-list.files(path=output_CONTROL_PREG_window_atc_2)
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-22)

# denominator is all covid+ pregnancies by trimester

my_PREG<-fread( paste0(matched_folder, "matches_pregnant_cov_neg.csv"))

denom_trim_1_all<-nrow(my_PREG[my_PREG$cov_trimester==1,])
denom_trim_2_all<-nrow(my_PREG[my_PREG$cov_trimester==2,])
denom_trim_3_all<-nrow(my_PREG[my_PREG$cov_trimester==3,])


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
  my_data<-fread(paste0(output_CONTROL_PREG_window_atc_2,my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  if(denom_trim_1_all>0){
  num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
  ci_min_1<-(prop.test(num_minus_1, denom_trim_1_all, alternative = "two.sided", correct=F))
  prop_minus_trim_1[i]<-(round(ci_min_1$estimate,2))*100
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

table_2def<-as.data.frame(cbind((my_names), (final_minus_trim_1),(final_plus_trim_1), 
                        (final_minus_trim_2),(final_plus_trim_2), (final_minus_trim_3), (final_plus_trim_3)))

colnames(table_2def)<-c("Drug", " -30 days (controls)", "+30 days (controls)", "-30 days (controls)", 
                      "+30 days (controls)","-30 days (controls)", "+30 days (controls)")


table1a_total<-IMPORT_PATTERN(pat="table_1_a", dir=final_output_dir)

table1b_nonsevere<-IMPORT_PATTERN(pat="table_1_b", dir=final_output_dir)

table1c_severe<-IMPORT_PATTERN(pat="table_1_c", dir=final_output_dir)

# d=trim 1 
# e=trim 2
# f=trim 3

cases_table2d<-cbind(my_names, table1a_total[,2:3],table1c_severe[,2:3], table1b_nonsevere[,2:3])
colnames_2d<-c("drug", "-30 days (total)","+30 days (total)", "-30 days (severe)","+30 days (severe)", "-30 days (nonsevere)","+30 days (nonsevere)")
colnames(cases_table2d)<-colnames_2d

table_2d<-cbind(cases_table2d, table_2def[,2:3])

fwrite(table_2d, paste0(final_output_dir,DAP, "_table_2_d.csv"))
######################################################################

cases_table2e<-cbind(my_names, table1a_total[,4:5],table1c_severe[,4:5], table1b_nonsevere[,4:5])
colnames_2e<-c("drug", "-30 days (total)","+30 days (total)", "-30 days (severe)","+30 days (severe)", "-30 days (nonsevere)","+30 days (nonsevere)")
colnames(cases_table2e)<-colnames_2e

table_2e<-cbind(my_names, cases_table2e, table_2def[,4:5])

fwrite(table_2e, paste0(final_output_dir,DAP, "_table_2_e.csv"))

######################################################################

cases_table2f<-cbind(my_names, table1a_total[,6:7],table1c_severe[,6:7], table1b_nonsevere[,6:7])
colnames_2f<-c("drug", "-30 days (total)","+30 days (total)", "-30 days (severe)","+30 days (severe)", "-30 days (nonsevere)","+30 days (nonsevere)")
colnames(cases_table2f)<-colnames_2f

table_2f<-cbind(cases_table2f, table_2def[,6:7])

fwrite(table_2f, paste0(final_output_dir,DAP, "_table_2_f.csv"))



print("final outputs with proportions and 95% confidence intervals are stored in g_output/final")
print("warnings are generated when count==0, ignore them, it's OK")

