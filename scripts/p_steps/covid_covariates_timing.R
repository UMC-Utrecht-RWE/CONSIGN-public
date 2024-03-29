# covaraite timing: brute force (actually doesn't take too long :) 

# need trimester and severity

'%exclude%' <- function(x,y)!('%in%'(x,y))

# need simulated end date of pregnancy for cov pos non-preg group

cov_pos_nonpreg<-fread(paste0(matched_folder, "matches_cov_pos_non_preg.csv"))
cov_pos_nonpreg$pregnancy_end_date<-cov_pos_nonpreg$pregnancy_start_date+280 #normal gestation period

fwrite(cov_pos_nonpreg, paste0(matched_folder, "matches_cov_pos_non_preg.csv"))

cohort_covariate_folders<-c(output_cov_neg_pan_preg, output_cov_pos_pan_preg, output_cov_pos_non_preg)


# we need the cov_date for each case and control
cohort_covid_data<-c(paste0(matched_folder, "matches_pregnant_cov_neg.csv"), 
                             paste0(matched_folder, "matches_cases.csv"), 
                             paste0(matched_folder, "matches_cov_pos_non_preg.csv"))


output_folders<-c(output_cov_pregnant_control, output_cov_cases, output_cov_nonpregnant_control)
cohort_names<-c("pregnant_cov_neg_control.csv", "cases.csv", "covid_positive_nonpregnant_control.csv")
# output should have the person_id of the control, and a column for each covariate with a 0/1 if they have this event within 1 year before covid_date

for(i in 1:length(cohort_covariate_folders)){
  
  my_tables<-list.files(path=cohort_covariate_folders[i])
  my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-4)
  my_names<-my_names[my_names%exclude%"vaccine"==T]
  print(my_names)
  my_covid_data<-fread(cohort_covid_data[i])

  my_output_df<- as.data.frame(matrix(data=NA, nrow=nrow(my_covid_data), ncol=(length(my_names)+3)))
  colnames(my_output_df)<-c("person_id","severity","covid_trimester", my_names)
  
  my_output_df$person_id<-my_covid_data$person_id
  my_output_df$covid_trimester<-my_covid_data$cov_trimester
  my_output_df$severity<-my_covid_data$severity
  
  
  for(j in 1:length(my_names)){
    my_covariate_data<-fread(paste0(cohort_covariate_folders[i], my_tables[j]))
    my_covariate_data<-my_covariate_data[complete.cases(my_covariate_data)==T,]
    for(p in 1:nrow(my_covid_data)){
      my_id<-my_covid_data$person_id[p]
      my_date<-my_covid_data$pregnancy_start_date[p]
      my_id_covariate_data<-my_covariate_data[my_covariate_data$person_id==my_id,]
      time_window<-my_id_covariate_data$date-my_date
      # all covariate signal dates - covid_date--> if any of these dates are between -365 and 0 --> covariate==1
      if(any(time_window<=0 & time_window>=-365)){covariate_result<-1}else{covariate_result<-0}
      my_output_df[p,j+3]<-covariate_result
    }
  
  }
  print(my_output_df)
 
  my_tables<-list.files(path=cohort_covariate_folders[i], pattern = "vaccine")
  my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-4)
  my_vaccine_output<-vector()
  
  for(j in 1:length(my_names)){
    my_covariate_data<-fread(paste0(cohort_covariate_folders[i], my_tables[j]))
    my_covariate_data<-my_covariate_data[complete.cases(my_covariate_data)==T,]
    for(p in 1:nrow(my_covid_data)){
      my_id<-my_covid_data$person_id[p]
      my_start_date<-my_covid_data$pregnancy_start_date[p]
      my_end_date<-my_covid_data$pregnancy_end_date[p]
      my_id_covariate_data<-my_covariate_data[my_covariate_data$person_id==my_id,]
      time_window_start<- my_id_covariate_data$date-(my_start_date)
      time_window_end<-my_end_date-(my_id_covariate_data$date)
      # all covariate signal dates - covid_date--> if any of these dates are between -365 and 0 --> covariate==1
      if(any(time_window_start>=0 & time_window_end>=0)){covariate_result<-1}else{covariate_result<-0}
      my_vaccine_output[p]<-covariate_result
    }
    
  }
my_vaccine_output

my_output_df$vaccine<-my_vaccine_output
  
  fwrite(my_output_df, paste0(output_folders[i],cohort_names[i]))
}

