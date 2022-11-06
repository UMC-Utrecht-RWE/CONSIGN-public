#Author:Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 01/03/2022

# tables 2abc maternality section (bottom)

# a= trimester 1
# b= trimiester 2
# c= trimester 3

# 3 case columns: total, severe, non severe

#all pregnant (covid negative) matches

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

case_data<-fread(paste0(g_output_mat_cov_pan_pos, "cases.csv"))

# gest_diab
# preeclamp
# adverse outcome (SB, SA)

case_data$SA_SB<-sum(case_data$Spont_Abort, case_data$Still_Birth)
case_data$SA_SB[case_data$SA_SB>0]<-1

#subset only maternal covariates for table 2abc

case_data<-case_data[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]

# create "any maternality" column


case_data$any<-rowSums(case_data[,4:ncol(case_data)])
case_data$any[case_data$any>0]<-1


case_data_a<-case_data[(case_data$covid_trimester==1),]
case_data_severe_a<-case_data_a[case_data_a$severity==1,]
case_data_nonsevere_a<-case_data_a[case_data_a$severity==0,]

case_data_b<-case_data[case_data$covid_trimester==2,]
case_data_severe_b<-case_data_b[case_data_b$severity==1,]
case_data_nonsevere_b<-case_data_b[case_data_b$severity==0,]

case_data_c<-case_data[case_data$covid_trimester==3,]
case_data_severe_c<-case_data_c[case_data_c$severity==1,]
case_data_nonsevere_c<-case_data_c[case_data_c$severity==0,]


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


####################################################

control_data<-fread(paste0(g_output_mat_cov_pan_neg, "covid_negative_pregnant_control.csv"))

control_data$SA_SB<-sum(control_data$Spont_Abort, control_data$Still_Birth)
control_data$SA_SB[control_data$SA_SB>0]<-1

#subset only maternal covariates for table 2abc

control_data<-control_data[,c("person_id", "severity", "covid_trimester","gest_diab", "Preeclampsia", "SA_SB")]

# create "any maternality" column


control_data$any<-rowSums(control_data[,4:ncol(control_data)])
control_data$any[control_data$any>0]<-1

####################################################

control_data_a<-control_data[control_data$covid_trimester==1,]
control_data_a<-control_data_a[,c(-1,-2,-3)]

control_data_b<-control_data[control_data$covid_trimester==2,]
control_data_b<-control_data_b[,c(-1,-2,-3)]

control_data_c<-control_data[control_data$covid_trimester==3,]
control_data_c<-control_data_c[,c(-1,-2,-3)]

##############################################


my_names<-colnames(case_data[,c(-1,-2,-3)])


#####################################################################
# case columns table a (trim 1)

my_data<-list(case_data_a, case_data_severe_a, case_data_nonsevere_a, control_data_a)

results_a<-as.data.frame(matrix(nrow=length(my_names), ncol=length(my_data)+1))

results_a[,1]<-my_names

colnames(results_a)<-c("maternal comorbidity", "all cases", "severe cases", "nonsevere cases", "all controls")

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


fwrite(results_a, paste0(final_output_dir, DAP,"_table2a_maternal.csv"))

#####################################################################
# case columns table b (trim 2)

my_data<-list(case_data_b, case_data_severe_b, case_data_nonsevere_b, control_data_b)

results_b<-as.data.frame(matrix(nrow=length(my_names), ncol=length(my_data)+1))

results_b[,1]<-my_names

colnames(results_b)<-c("maternal comorbidity", "all cases", "severe cases", "nonsevere cases", "all controls")

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


fwrite(results_b, paste0(final_output_dir, DAP,"_table2b_maternal.csv"))

#####################################################################
# case columns table c (trim 3)

my_data<-list(case_data_c, case_data_severe_c, case_data_nonsevere_c, control_data_c)

results_c<-as.data.frame(matrix(nrow=length(my_names), ncol=length(my_data)+1))

results_c[,1]<-my_names

colnames(results_c)<-c("maternal comorbidity", "all cases", "severe cases", "nonsevere cases", "all controls")

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


fwrite(results_c,paste0(final_output_dir, DAP,"_table2c_maternal.csv"))
