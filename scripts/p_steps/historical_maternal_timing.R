# covaraite timing: brute force (actually doesn't take too long :) 

# maternal covariates -within 2 years BEFORE start of index pregnancy

# need trimester and severity
hist_preg<-fread(paste0(hist_preg_folder,"my_PREG.csv"))
hist_preg$severity<-NA
hist_preg$cov_trimester<-NA
fwrite(hist_preg, paste0(hist_preg_folder,"my_PREG.csv"))



hist_preg<-fread(paste0(hist_preg_folder, "my_PREG.csv"))


my_tables<-list.files(output_mat_cov_hist)
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-4)
print(my_names)


my_output_df<- as.data.frame(matrix(data=NA, nrow=nrow(hist_preg), ncol=(length(my_names)+3)))
colnames(my_output_df)<-c("person_id","severity","covid_trimester", my_names)

my_output_df$person_id<-hist_preg$person_id
my_output_df$covid_trimester<-hist_preg$cov_trimester
my_output_df$severity<-hist_preg$severity

# change start preg to end preg to pick up events that happen during historical pregnancy

for(j in 1:length(my_tables)){
  my_covariate_data<-fread(paste0(output_mat_cov_hist, my_tables[j]))
  my_covariate_data<-my_covariate_data[complete.cases(my_covariate_data)==T,]
  for(p in 1:nrow(hist_preg)){
    my_id<-hist_preg$person_id[p]
    my_date_end<-hist_preg$pregnancy_end_date[p]
    my_date_start<-hist_preg$pregnancy_start_date[p]
    my_id_covariate_data<-my_covariate_data[my_covariate_data$id==my_id,]
    # event date needs to be greater than start date and less than end date
    time_window_end<-my_id_covariate_data$date-my_date_end 
    time_window_start<-my_id_covariate_data$date-my_date_start
    # all covariate signal dates - covid_date--> if any of these dates are between -365 and 0 --> covariate==1
    if(any(time_window_end<=0 & time_window_start>=0)){covariate_result<-1}else{covariate_result<-0}
    my_output_df[p,j+3]<-covariate_result
  }
  
}
print(my_output_df)
fwrite(my_output_df, paste0(g_output_mat_cov_hist,"historical.csv"))
