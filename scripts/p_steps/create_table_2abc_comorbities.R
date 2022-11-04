#Author:Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 01/03/2022

# tables 2abc comorbidity section (bottom)

# a= trimester 1
# b= trimiester 2
# c= trimester 3

# 3 case columns: total, severe, non severe

#all pregnant (covid negative) matches

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

case_data<-fread(paste0(output_cov_cases, "cases.csv"))
case_data_severe<-case_data[case_data$severity==1,]
case_data_nonsevere<-case_data[case_data$severity==0,]

# remove non-covariate columns
case_data<-case_data[,c(-1,-2,-3)]

control_data<-fread(paste0(output_cov_pregnant_control, "pregnant_cov_neg_control.csv"))
control_data_severe<-control_data[control_data$severity==1,]
control_data_nonsevere<-control_data[control_data$severity==0,]

my_names<-colnames(case_data)
my_names<-my_names[4:length(my_names)]

#####################################################################
# first column



total_cases_sums<-as.vector(colSums(case_data))

prop_test_case<-list()
case_prop<-list()
case_lo_ci<-list()
case_hi_ci<-list()
for (i in 1:ncol(case_data)){
my_prop_test<-prop.test(total_cases_sums[i], nrow(case_data))
case_prop[i]<-(round(my_prop_test$estimate,3))*100
case_lo_ci[i]<-(round(my_prop_test$conf.int[1],3))*100
case_hi_ci[i]<-(round(my_prop_test$conf.int[2],3))*100
}
print("warnings generated with count of 0- not a problem- don't worry")

results_case_total<-paste0(case_prop, "(", case_lo_ci,"-", case_hi_ci,")")

#########################################################################
# second column 
case_data_severe<-case_data_severe[,c(-1,-2,-3)]
total_cases_sums<-as.vector(colSums(case_data))

prop_test_case<-list()
case_prop<-list()
case_lo_ci<-list()
case_hi_ci<-list()
for (i in 1:ncol(case_data)){
  my_prop_test<-prop.test(total_cases_sums[i], nrow(case_data))
  case_prop[i]<-(round(my_prop_test$estimate,3))*100
  case_lo_ci[i]<-(round(my_prop_test$conf.int[1],3))*100
  case_hi_ci[i]<-(round(my_prop_test$conf.int[2],3))*100
}
print("warnings generated with count of 0- not a problem- don't worry")

results_case_total<-paste0(case_prop, "(", case_lo_ci,"-", case_hi_ci,")")




