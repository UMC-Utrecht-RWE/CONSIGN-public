# table 6 drug utilization and outcome for CASES ONLY
# LBW 
# LBW 
# gestdiab 
# lbw

# need to combine the maternal covariate data with the drug utilization data. 

# for the neonatal- need to use the mother child link from table 5

my_drug_files<-list.files(output_cov_window_atc_2)

my_drug_names<-str_sub(unlist(my_drug_files), 1, str_length(unlist(my_drug_files))-22)

DU_LBW_outcome<-as.data.frame(matrix(ncol=10, nrow=length(my_drug_names)))

DU_LBW_outcome[,1]<-my_drug_names

colnames(DU_LBW_outcome)<-c("drug group", "(T1) Exposed to medication in 30 days pre COVID-19 positive test/diagnosis",
                                  "(T1) Not-exposed  to medication in 30 days post COVID-19 positive test/diagnosis",
                                  "(T1) Exposed to medication in 30 days post COVID-19 positive test/diagnosis", 
                                "(T2) Exposed to medication in 30 days pre COVID-19 positive test/diagnosis",
                                "(T2) Not-exposed  to medication in 30 days post COVID-19 positive test/diagnosis",
                                "(T2) Exposed to medication in 30 days post COVID-19 positive test/diagnosis",
                                "(T3) Exposed to medication in 30 days pre COVID-19 positive test/diagnosis",
                                "(T3) Not-exposed  to medication in 30 days post COVID-19 positive test/diagnosis",
                                "(T3) Exposed to medication in 30 days post COVID-19 positive test/diagnosis")

# column A :
# denominator is number of women who were exposed to drug in -30 window == SUM of minus_30 column
# numerator is the number of women who were exposed in -30 days AND have the outcome
# column B:
# denominator is number of women who were exposed NOT to drug in +30 window- == number in total cohort for the trimester subset (ex. T1_20) MINUS the exposed in +30 days
# numerator: the number of women unexposed in +30 days who have the outcome
# column C:
# denominator is number of women who were exposed to drug in +30 window== SUM of +30 column
# numerator:  is the number of women who were exposed in +30 days AND have the outcome


# gestational diabetes: 
# restrict to pregnancies with 20 weeks
# only trimeser 1

my_LBW_data_list<- list(T1_case_neo_outcome[,c("mom_id", "LBW")], 
                            T2_case_neo_outcome[,c("mom_id", "LBW")],
                            T3_case_neo_outcome[,c("mom_id", "LBW")])
my_cols<-list(c(2:4), c(5:7), c(8:10))
for(j in 1:3){
input_col<-my_cols[[j]]
my_LBW_data<-my_LBW_data_list[[j]]
my_cohort_size<-nrow(my_LBW_data)


for(i in 1:length(my_drug_names)){
results<-list()
my_drug_data<-fread(paste0(output_cov_window_atc_2, my_drug_files[[i]]))
my_drug_data<-my_drug_data[my_drug_data$cov_trimester==j,]
my_drug_data$mom_id<-my_drug_data$person_id
# make sure drug exposure columns are numeric
my_drug_data$minus_30<-as.numeric(my_drug_data$minus_30)
my_drug_data$plus_30<-as.numeric(my_drug_data$plus_30)
# need to match the --MOM-- ID because not all pregnancies will be end in LB (to be included in denominator)
my_drug_data<-my_drug_data[my_drug_data$mom_id%in%my_LBW_data$mom_id,]
my_denom_A<-sum(my_drug_data$minus_30)
my_denom_B<-(my_cohort_size)-(sum(my_drug_data$plus_30))
my_denom_C<-sum(my_drug_data$plus_30)

# select LBW cases from neonate data
  my_LBW_data<-my_LBW_data[my_LBW_data$LBW==1,]
my_LBW_drug<-unique(merge(my_LBW_data, my_drug_data, by="mom_id"))
my_numer_minus30<-nrow(my_LBW_drug[my_LBW_drug$minus_30==1,])
my_numer_plus30<-nrow(my_LBW_drug[my_LBW_drug$plus_30==1,])
my_numer_nonexposed_plus30<-nrow(my_LBW_drug[my_LBW_drug$plus_30==0,])
if(my_denom_A>0){
  my_prop_test_minus30<-prop.test(my_numer_minus30, my_denom_A)
  results[[1]]<-paste0((round(my_prop_test_minus30$estimate,3)*100)," (", (round(my_prop_test_minus30$conf.int[1],3)*100),"-",(round(my_prop_test_minus30$conf.int[2],3)*100),")")
  }else{results[[1]]<-"no matches"}
if(my_denom_B>0){  
   my_prop_test_nonexposed_plus30<-prop.test(my_numer_nonexposed_plus30, my_denom_B)
  results[[2]]<-paste0((round(my_prop_test_nonexposed_plus30$estimate,3)*100)," (", (round(my_prop_test_nonexposed_plus30$conf.int[1],3)*100),"-",(round(my_prop_test_nonexposed_plus30$conf.int[2],3)*100),")")
  }else{results[[2]]<-"no matches"}
if(my_denom_C>0){  
  my_prop_test_plus30<-prop.test(my_numer_plus30, my_denom_C)
  results[[3]]<-paste0((round(my_prop_test_plus30$estimate,3)*100)," (", (round(my_prop_test_plus30$conf.int[1],3)*100),"-",(round(my_prop_test_plus30$conf.int[2],3)*100),")")
}else{results[[3]]<-"no matches"}
DU_LBW_outcome[i,input_col]<-unlist(results)
}

}
fwrite(DU_LBW_outcome, paste0(final_output_dir, "table_6_LBW.csv"))
