# covaraite timing: brute force (actually doesn't take too long :) 

# maternal covariates -within 2 years BEFORE start of index pregnancy

# need trimester and severity
hist_preg<-fread(paste0(hist_preg_folder,"my_PREG.csv"))
hist_preg$severity<-NA
hist_preg$cov_trimester<-NA
fwrite(hist_preg, paste0(hist_preg_folder,"my_PREG.csv"))


cohort_covariate_folders<-c(output_mat_cov_pan_neg, output_mat_cov_pan_pos, output_mat_cov_hist)

# we need the cov_date for each case and control
cohort_preg_data<-c(paste0(matched_folder, "matches_pregnant_cov_neg.csv"), 
                             paste0(matched_folder, "matches_cases.csv"), 
                             paste0(hist_preg_folder,"my_PREG.csv"))


output_folders<-c(g_output_mat_cov_pan_neg, g_output_mat_cov_pan_pos,output_mat_cov_hist)
cohort_names<-c("covid_negative_pregnant_control.csv", "cases.csv", "historical.csv")
# output should have the person_id of the control, and a column for each covariate with a 0/1 if they have this event within 1 year before covid_date

for(i in 1:length(cohort_covariate_folders)){
  
  my_tables<-list.files(path=cohort_covariate_folders[i])
  my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-4)
  print(my_names)
  my_preg_data<-fread(cohort_preg_data[i])

  my_output_df<- as.data.frame(matrix(data=NA, nrow=nrow(my_preg_data), ncol=(length(my_names)+3)))
  colnames(my_output_df)<-c("person_id","severity","covid_trimester", my_names)
  
  my_output_df$person_id<-my_preg_data$person_id
  my_output_df$covid_trimester<-my_preg_data$cov_trimester
  my_output_df$severity<-my_preg_data$severity
  
  
  for(j in 1:length(my_tables)){
    my_covariate_data<-fread(paste0(cohort_covariate_folders[i], my_tables[j]))
    my_covariate_data<-my_covariate_data[complete.cases(my_covariate_data)==T,]
    for(p in 1:nrow(my_preg_data)){
      my_id<-my_preg_data$person_id[p]
      my_date<-my_preg_data$pregnancy_start_date[p]
      my_id_covariate_data<-my_covariate_data[my_covariate_data$person_id==my_id,]
      time_window<-my_id_covariate_data$date-my_date
      # all covariate signal dates - covid_date--> if any of these dates are between -365 and 0 --> covariate==1
      if(any(time_window<=0 & time_window>=-730)){covariate_result<-1}else{covariate_result<-0}
      my_output_df[p,j+3]<-covariate_result
    }
  
  }
  print(my_output_df)
  fwrite(my_output_df, paste0(output_folders[i],cohort_names[i]))
}

