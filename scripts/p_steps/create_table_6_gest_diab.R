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

# gestational diabetes: 
# restrict to pregnancies with 20 weeks
# only trimeser 1

my_GD_data<- T1_20_case_mat_outcome[,c("person_id", "gest_diab")]
my_GD_data<-my_GD_data[my_GD_data$gest_diab==1,]

for(i in 1:length(my_drug_names)){
results<-list()
my_drug_data<-fread(paste0(output_cov_window_atc_2, my_drug_files[[i]]))
my_drug_data<-my_drug_data[my_drug_data$cov_trimester==1,]
my_denom<-nrow(my_drug_data)
if(my_denom>0){
my_GD_drug<-merge(my_GD_data, my_drug_data, by="person_id")
my_numer_minus30<-nrow(my_GD_drug[my_GD_drug$minus_30==1,])
my_numer_plus30<-nrow(my_GD_drug[my_GD_drug$plus_30==1,])
my_numer_nonexposed_plus30<-nrow(my_GD_drug[my_GD_drug$plus_30==0,])

  my_prop_test_minus30<-prop.test(my_numer_minus30, my_denom)
  results[[1]]<-paste0((round(my_prop_test_minus30$estimate,3)*100)," (", (round(my_prop_test_minus30$conf.int[1],3)*100),"-",(round(my_prop_test_minus30$conf.int[2],3)*100),")")
  my_prop_test_nonexposed_plus30<-prop.test(my_numer_nonexposed_plus30, my_denom)
  results[[2]]<-paste0((round(my_prop_test_nonexposed_plus30$estimate,3)*100)," (", (round(my_prop_test_nonexposed_plus30$conf.int[1],3)*100),"-",(round(my_prop_test_nonexposed_plus30$conf.int[2],3)*100),")")
  my_prop_test_plus30<-prop.test(my_numer_plus30, my_denom)
  results[[3]]<-paste0((round(my_prop_test_plus30$estimate,3)*100)," (", (round(my_prop_test_plus30$conf.int[1],3)*100),"-",(round(my_prop_test_plus30$conf.int[2],3)*100),")")
}else{results[[1]]<-"no matches"
results[[2]]<-"no matches"
results[[3]]<-"no matches"}
DU_gest_diab_outcome[i,2:4]<-unlist(results)
}

fwrite(DU_gest_diab_outcome, paste0(final_output_dir, "table_6_gestdiab.csv"))
