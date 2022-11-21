# table 6 drug utilization and outcome for CASES ONLY
# preeclamp 
# preterm 
# gestdiab 
# lbw

# need to combine the maternal covariate data with the drug utilization data. 

# for the neonatal- need to use the mother child link from table 5



my_drug_files<-list.files(output_cov_window_atc_2)

my_drug_names<-str_sub(unlist(my_drug_files), 1, str_length(unlist(my_drug_files))-22)

DU_gest_diab_outcome<-as.data.frame(matrix(ncol=4, nrow=length(my_drug_names)))

DU_gest_diab_outcome[,1]<-my_drug_names

colnames(DU_gest_diab_outcome)<-c("drug group", "(a) Exposed to medication in 30 days pre COVID-19 positive test/diagnosis",
                                  "(b) Not-exposed  to medication in 30 days post COVID-19 positive test/diagnosis",
                                  "(c) Exposed to medication in 30 days post COVID-19 positive test/diagnosis")

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




for(i in 1:length(my_drug_names)){
  my_GD_data<- T1_20_case_mat_outcome[,c("person_id", "gest_diab")]
  my_cohort_size<-nrow(my_GD_data)
results<-list()
my_drug_data<-fread(paste0(output_cov_window_atc_2, my_drug_files[[i]]), select = c("person_id","minus_30", "plus_30", "severity", "trimester"))
if(nrow(my_drug_data)>0){
# select trimester
my_drug_data<-my_drug_data[my_drug_data$cov_trimester==1,]
# make sure drug exposure columns are numeric
my_drug_data$minus_30<-as.numeric(my_drug_data$minus_30)
my_drug_data$plus_30<-as.numeric(my_drug_data$plus_30)
# need to match the ID because not all T1 pregnancies will be in T1_20
my_drug_data<-my_drug_data[my_drug_data$person_id%in%my_GD_data$person_id,]
my_denom_A<-sum(my_drug_data$minus_30)
my_denom_B<-(my_cohort_size)-(sum(my_drug_data$plus_30))
my_denom_C<-sum(my_drug_data$plus_30)

# select only those with maternal outcome
my_GD_data<-my_GD_data[my_GD_data$gest_diab==1,]
my_drug_data<-my_drug_data[my_drug_data$person_id%in%my_GD_data$person_id,]
my_GD_drug<-merge(my_GD_data, my_drug_data, by="person_id")
my_numer_minus30<-nrow(my_GD_drug[my_GD_drug$minus_30==1,])
my_numer_nonexposed_plus30<-nrow(my_GD_drug[my_GD_drug$plus_30==0,])
my_numer_plus30<-nrow(my_GD_drug[my_GD_drug$plus_30==1,])

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
}else{results[[3]]<-"no matches"}}else{results[[1]]<-NA

my_prop_test_nonexposed_plus30<-prop.test(0, my_cohort_size)
results[[2]]<-paste0((round(my_prop_test_nonexposed_plus30$estimate,3)*100)," (", (round(my_prop_test_nonexposed_plus30$conf.int[1],3)*100),"-",(round(my_prop_test_nonexposed_plus30$conf.int[2],3)*100),")")

results[[3]]<-NA}
DU_gest_diab_outcome[i,2:4]<-unlist(results)
}

fwrite(DU_gest_diab_outcome, paste0(final_output_dir, "table_6_gestdiab.csv"))
