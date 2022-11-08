# covaraite timing: brute force (actually doesn't take too long :) 

# maternal covariates -within 1 year AFTER start of index pregnancy

# need trimester and severity

cohort_covariate_folders<-c(output_neonates_case,output_neonates_control, output_neonates_hist)

# we need the cov_date for each case and control
cohort_child_data<-c(paste0(case_neonate_folder,"case_neonates.csv"), 
                             paste0(control_neonate_folder, "control_neonates.csv"), 
                             paste0(historical_neonate_folder,"historical_neonates.csv"))


output_folders<-c(output_neonates_case, output_neonates_control, output_neonates_hist)
cohort_names<-c("case_neonates_outcomes.csv", "control_neonates_outcomes.csv", "hist_neonates_outcomes.csv")
# output should have the person_id of the control, and a column for each covariate with a 0/1 if they have this event within 1 year before covid_date

for(i in 1:length(cohort_covariate_folders)){
  
  my_tables<-list.files(path=cohort_covariate_folders[i], pattern = ".csv")
  my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-4)
  print(my_names)
  my_child_data<-fread(cohort_child_data[i])
  
  

  my_output_df<- as.data.frame(matrix(data=NA, nrow=nrow(my_child_data), ncol=(length(my_names)+1)))
  colnames(my_output_df)<-c("person_id", my_names)
  
  my_output_df$person_id<-my_child_data$person_id
  

  
  for(j in 1:length(my_tables)){
    my_covariate_data<-fread(paste0(cohort_covariate_folders[i], my_tables[j]))
    date<-as.Date(as.character(my_covariate_data$date), format = "%Y%m%d")
    date_num<-as.numeric(date)
    my_covariate_data$date<-date_num
    print(my_names[j])
    print(my_covariate_data)
    my_covariate_data<-my_covariate_data[complete.cases(my_covariate_data)==T,]
    for(p in 1:nrow(my_child_data)){
      my_id<-my_child_data$person_id[p]
      my_date<-my_child_data$DOB[p]
      my_id_covariate_data<-my_covariate_data[my_covariate_data$id==my_id,]
      time_window<-my_id_covariate_data$date-my_date
      # all covariate signal dates - covid_date--> if any of these dates are between -365 and 0 --> covariate==1
      if(any(time_window<=90 & time_window>=0)){covariate_result<-1}else{covariate_result<-0}
      my_output_df[p,j+1]<-covariate_result
    }
  
  }
  print(my_output_df)
  fwrite(my_output_df, paste0(output_folders[i],cohort_names[i]))
}

