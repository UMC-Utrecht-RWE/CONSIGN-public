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

# add standardized difference between severe and non-severe
# The standardized difference is defined as the difference between the mean divided by the overall standard deviation.


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

case_data<-fread(paste0(output_cov_cases, "cases.csv"))
# vaccination separate
case_vaccine<-case_data[,c("severity", "covid_trimester","vaccine")]
case_data<-case_data[,-"vaccine"]

# create "any comorbidity" column

case_data$any<-rowSums(case_data[,4:ncol(case_data)])
case_data$any[case_data$any>0]<-1


case_data_a<-case_data[(case_data$covid_trimester==1),]
case_data_severe_a<-case_data_a[case_data_a$severity==1,]
case_data_nonsevere_a<-case_data_a[case_data_a$severity==0,]

case_vaccine_a<-case_vaccine[(case_vaccine$covid_trimester==1),]
case_vaccine_severe_a<-case_vaccine_a[case_vaccine_a$severity==1,]
case_vaccine_nonsevere_a<-case_vaccine_a[case_vaccine_a$severity==0,]

case_data_b<-case_data[case_data$covid_trimester==2,]
case_data_severe_b<-case_data_b[case_data_b$severity==1,]
case_data_nonsevere_b<-case_data_b[case_data_b$severity==0,]

case_vaccine_b<-case_vaccine[(case_vaccine$covid_trimester==2),]
case_vaccine_severe_b<-case_vaccine_b[case_vaccine_b$severity==1,]
case_vaccine_nonsevere_b<-case_vaccine_b[case_vaccine_b$severity==0,]

case_data_c<-case_data[case_data$covid_trimester==3,]
case_data_severe_c<-case_data_c[case_data_c$severity==1,]
case_data_nonsevere_c<-case_data_c[case_data_c$severity==0,]

case_vaccine_c<-case_vaccine[(case_vaccine$covid_trimester==3),]
case_vaccine_severe_c<-case_vaccine_c[case_vaccine_c$severity==1,]
case_vaccine_nonsevere_c<-case_vaccine_c[case_vaccine_c$severity==0,]

###########################################
# remove non-covariate columns

case_data_a<-case_data_a[,c(-1,-2,-3)]
case_data_severe_a<-case_data_severe_a[,c(-1,-2,-3)]
case_data_nonsevere_a<-case_data_nonsevere_a[,c(-1,-2,-3)]

case_data_b<-case_data_b[,c(-1,-2,-3)]
case_data_severe_b<-case_data_severe_b[,c(-1,-2,-3)]
case_data_nonsevere_b<-case_data_nonsevere_b[,c(-1,-2,-3)]

case_data_c<-case_data_c[,c(-1,-2,-3)]
case_data_severe_c<-case_data_severe_c[,c(-1,-2,-3)]
case_data_nonsevere_c<-case_data_nonsevere_c[,c(-1,-2,-3)]

###############################################################

case_vaccine_a<-case_vaccine_a[,c(-1,-2)]
case_vaccine_severe_a<-case_vaccine_severe_a[,c(-1,-2)]
case_vaccine_nonsevere_a<-case_vaccine_nonsevere_a[,c(-1,-2)]

case_vaccine_b<-case_vaccine_b[,c(-1,-2)]
case_vaccine_severe_b<-case_vaccine_severe_b[,c(-1,-2)]
case_vaccine_nonsevere_b<-case_vaccine_nonsevere_b[,c(-1,-2)]

case_vaccine_c<-case_vaccine_c[,c(-1,-2)]
case_vaccine_severe_c<-case_vaccine_severe_c[,c(-1,-2)]
case_vaccine_nonsevere_c<-case_vaccine_nonsevere_c[,c(-1,-2)]

####################################################

control_data<-fread(paste0(output_cov_pregnant_control, "pregnant_cov_neg_control.csv"))
control_vaccine<-control_data[,c("severity", "covid_trimester", "vaccine")]
control_data<-control_data[,-"vaccine"]

control_data$any<-rowSums(control_data[,4:ncol(control_data)])
control_data$any[control_data$any>0]<-1

control_data_a<-control_data[control_data$covid_trimester==1,]
control_data_a<-control_data_a[,c(-1,-2,-3)]

control_data_b<-control_data[control_data$covid_trimester==2,]
control_data_b<-control_data_b[,c(-1,-2,-3)]

control_data_c<-control_data[control_data$covid_trimester==3,]
control_data_c<-control_data_c[,c(-1,-2,-3)]

##############################################

control_vaccine_a<-control_vaccine[control_vaccine$covid_trimester==1,]
control_vaccine_a<-control_vaccine_a[,c(-1,-2)]

control_vaccine_b<-control_vaccine[control_vaccine$covid_trimester==2,]
control_vaccine_b<-control_vaccine_b[,c(-1,-2)]

control_vaccine_c<-control_vaccine[control_vaccine$covid_trimester==3,]
control_vaccine_c<-control_vaccine_c[,c(-1,-2)]

# no severity- these controls didn't have covid 

my_names<-colnames(case_data[,c(-1,-2,-3)])


#####################################################################
# case columns table a (trim 1)

my_data<-list(case_data_a, case_data_severe_a, case_data_nonsevere_a, control_data_a)

results_a<-as.data.frame(matrix(nrow=length(my_names), ncol=length(my_data)+1))

results_a[,1]<-my_names

colnames(results_a)<-c("comorbidity", "all cases", "severe cases", "nonsevere cases", "all controls")

for(j in 1:length(my_data)){
my_df<-my_data[[j]] 
print(j)
if((nrow(my_df)==0)==T){  
    results<-rep("no records", length(my_names))
        print(paste0(j,": no records in this group"))
            }else{print("records found for this group")
              if(nrow(my_df>1)){total_cases_sums<-colSums(my_df)} else{(total_cases_sums<-my_df[1,])}

            my_percent<-(total_cases_sums/nrow(my_df))
            my_percent<-round(my_percent,3)
            my_percent<-my_percent*100

results<-paste0(total_cases_sums,"(", my_percent,"%)")}
results_a[,(j+1)]<-results}

####################################################################


my_vaccine<-list(case_vaccine_a, case_vaccine_severe_a, case_vaccine_nonsevere_a, control_vaccine_a)
vaccine_a<-vector()

for(j in 1:length(my_vaccine)){
  my_df<-my_vaccine[[j]] 
  print(j)
  if((nrow(my_df)==0)==T){  
    results<-rep("no records", length(my_names))
    print(paste0(j,": no records in this group"))
  }else{print("records found for this group")
    if(nrow(my_df>1)){total_cases_sums<-colSums(my_df)} else{(total_cases_sums<-my_df[1,])}
    
    my_percent<-(total_cases_sums/nrow(my_df))
    my_percent<-round(my_percent,3)
    my_percent<-my_percent*100
    
    results<-paste0(total_cases_sums,"(", my_percent,"%)")}
  vaccine_a[j]<-results}

vaccine_a<-c("flu vaccine", vaccine_a)


results_a<-rbind(results_a, vaccine_a)

#############################################################################
# standardized difference


results_sdif_case<-vector()


if((nrow(case_data_severe_a)>0)&(nrow(case_data_nonsevere_a)>0)){
  for(i in 1:length(my_names)){
    mycohort1<-(as.data.frame(case_data_severe_a)[,i])
    # print(mycohort1)
    mycohort2<-(as.data.frame(case_data_nonsevere_a)[,i])
    # print(mycohort2)
    results_sdif_case[i]<-standard_diff(cohort1 = mycohort1, cohort2 = mycohort2)}
}else{results_sdif_case<-rep("one or both cohorts empty", length(my_names))}



# vaccines 

results_vac_sdif_case<-vector()

# case vaccine

if((nrow(case_vaccine_severe_a)>0)&(nrow(case_vaccine_nonsevere_a)>0)){
  mycohort1<-unlist((as.data.frame(case_vaccine_severe_a))[,1])
  print(mycohort1)
  mycohort2<-unlist((as.data.frame(case_vaccine_nonsevere_a))[,1])
  print(mycohort2)
  results_vac_sdif_case<-standard_diff(cohort1 = mycohort1, cohort2 = mycohort2)
}else{results_vac_sdif_case<-("one or both cohorts empty")}




st_dif_case<-c(results_sdif_case, results_vac_sdif_case)


results_a$standard_diff_cases<-st_dif_case


fwrite(results_a, paste0(final_output_dir, DAP,"_table2a_comorbid.csv"))

#####################################################################
# case columns table b (trim 2)

my_data<-list(case_data_b, case_data_severe_b, case_data_nonsevere_b, control_data_b)

results_b<-as.data.frame(matrix(nrow=length(my_names), ncol=length(my_data)+1))

results_b[,1]<-my_names

colnames(results_b)<-c("comorbidity", "all cases", "severe cases", "nonsevere cases", "all controls")

for(j in 1:length(my_data)){
  my_df<-my_data[[j]] 
  print(j)
  if((nrow(my_df)==0)==T){  
    results<-rep("no records", length(my_names))
    print(paste0(j,": no records in this group"))
  }else{print("records found for this group")
    if(nrow(my_df>1)){total_cases_sums<-colSums(my_df)} else{(total_cases_sums<-my_df[1,])}
    
    my_percent<-(total_cases_sums/nrow(my_df))
    my_percent<-round(my_percent,3)
    my_percent<-my_percent*100
    
    results<-paste0(total_cases_sums,"(", my_percent,"%)")}
  results_b[,(j+1)]<-results}

####################################################################


my_vaccine<-list(case_vaccine_b, case_vaccine_severe_b, case_vaccine_nonsevere_b, control_vaccine_b)
vaccine_b<-vector()

for(j in 1:length(my_vaccine)){
  my_df<-my_vaccine[[j]] 
  print(j)
  if((nrow(my_df)==0)==T){  
    results<-rep("no records", length(my_names))
    print(paste0(j,": no records in this group"))
  }else{print("records found for this group")
    if(nrow(my_df>1)){total_cases_sums<-colSums(my_df)} else{(total_cases_sums<-my_df[1,])}
    
    my_percent<-(total_cases_sums/nrow(my_df))
    my_percent<-round(my_percent,3)
    my_percent<-my_percent*100
    
    results<-paste0(total_cases_sums,"(", my_percent,"%)")}
  vaccine_b[j]<-results}

vaccine_b<-c("flu vaccine", vaccine_b)


results_b<-rbind(results_b, vaccine_b)

#############################################################################
# standardized difference


results_sdif_case<-vector()


if((nrow(case_data_severe_b)>0)&(nrow(case_data_nonsevere_b)>0)){
  for(i in 1:length(my_names)){
    mycohort1<-(as.data.frame(case_data_severe_b)[,i])
    # print(mycohort1)
    mycohort2<-(as.data.frame(case_data_nonsevere_b)[,i])
    # print(mycohort2)
    results_sdif_case[i]<-standard_diff(cohort1 = mycohort1, cohort2 = mycohort2)}
}else{results_sdif_case<-rep("one or both cohorts empty", length(my_names))}



# vaccines 

results_vac_sdif_case<-vector()

# case vaccine

if((nrow(case_vaccine_severe_b)>0)&(nrow(case_vaccine_nonsevere_b)>0)){
  mycohort1<-unlist((as.data.frame(case_vaccine_severe_b))[,1])
  print(mycohort1)
  mycohort2<-unlist((as.data.frame(case_vaccine_nonsevere_b))[,1])
  print(mycohort2)
  results_vac_sdif_case<-standard_diff(cohort1 = mycohort1, cohort2 = mycohort2)
}else{results_vac_sdif_case<-("one or both cohorts empty")}




st_dif_case<-c(results_sdif_case, results_vac_sdif_case)


results_b$standard_diff_cases<-st_dif_case


fwrite(results_b, paste0(final_output_dir, DAP,"_table2b_comorbid.csv"))

#####################################################################
# case columns table c (trim 3)

my_data<-list(case_data_c, case_data_severe_c, case_data_nonsevere_c, control_data_c)

results_c<-as.data.frame(matrix(nrow=length(my_names), ncol=length(my_data)+1))

results_c[,1]<-my_names

colnames(results_c)<-c("comorbidity", "all cases", "severe cases", "nonsevere cases", "all controls")

for(j in 1:length(my_data)){
  my_df<-my_data[[j]] 
  print(j)
  if((nrow(my_df)==0)==T){  
    results<-rep("no records", length(my_names))
    print(paste0(j,": no records in this group"))
  }else{print("records found for this group")
    if(nrow(my_df>1)){total_cases_sums<-colSums(my_df)} else{(total_cases_sums<-my_df[1,])}
    
    my_percent<-(total_cases_sums/nrow(my_df))
    my_percent<-round(my_percent,3)
    my_percent<-my_percent*100
    
    results<-paste0(total_cases_sums,"(", my_percent,"%)")}
  results_c[,(j+1)]<-results}

####################################################################


my_vaccine<-list(case_vaccine_c, case_vaccine_severe_c, case_vaccine_nonsevere_c, control_vaccine_c)
vaccine_c<-vector()

for(j in 1:length(my_vaccine)){
  my_df<-my_vaccine[[j]] 
  print(j)
  if((nrow(my_df)==0)==T){  
    results<-rep("no records", length(my_names))
    print(paste0(j,": no records in this group"))
  }else{print("records found for this group")
    if(nrow(my_df>1)){total_cases_sums<-colSums(my_df)} else{(total_cases_sums<-my_df[1,])}
    
    my_percent<-(total_cases_sums/nrow(my_df))
    my_percent<-round(my_percent,3)
    my_percent<-my_percent*100
    
    results<-paste0(total_cases_sums,"(", my_percent,"%)")}
  vaccine_c[j]<-results}

vaccine_c<-c("flu vaccine", vaccine_c)


results_c<-rbind(results_c, vaccine_c)

#############################################################################
# standardized difference


results_sdif_case<-vector()


if((nrow(case_data_severe_c)>0)&(nrow(case_data_nonsevere_c)>0)){
  for(i in 1:length(my_names)){
    mycohort1<-(as.data.frame(case_data_severe_c)[,i])
    # print(mycohort1)
    mycohort2<-(as.data.frame(case_data_nonsevere_c)[,i])
    # print(mycohort2)
    results_sdif_case[i]<-standard_diff(cohort1 = mycohort1, cohort2 = mycohort2)}
}else{results_sdif_case<-rep("one or both cohorts empty", length(my_names))}



# vaccines 

results_vac_sdif_case<-vector()

# case vaccine

if((nrow(case_vaccine_severe_c)>0)&(nrow(case_vaccine_nonsevere_c)>0)){
  mycohort1<-unlist((as.data.frame(case_vaccine_severe_c))[,1])
  print(mycohort1)
  mycohort2<-unlist((as.data.frame(case_vaccine_nonsevere_c))[,1])
  print(mycohort2)
  results_vac_sdif_case<-standard_diff(cohort1 = mycohort1, cohort2 = mycohort2)
}else{results_vac_sdif_case<-("one or both cohorts empty")}




st_dif_case<-c(results_sdif_case, results_vac_sdif_case)


results_c$standard_diff_cases<-st_dif_case


fwrite(results_c,paste0(final_output_dir, DAP,"_table2c_comorbid.csv"))
