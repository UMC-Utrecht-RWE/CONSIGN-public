#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#26/10/2022

# covid covariates lookback

exposure_folders<-c(output_mat_cov_pan_neg, output_mat_cov_hist, output_mat_cov_pan_pos)

matched_data_folders<-c(matched_folder, hist_preg_folder, cov_pos_pan_preg_folder)

preg_data_names<-c("matches_pregnant_cov_neg.csv", "my_PREG.csv", "cov_pos_preg.csv")

output_folders<-list(g_output_mat_cov_pan_neg, g_output_mat_cov_hist,g_output_mat_cov_pan_pos)

for(i in 1:length(exposure_folders)){
  data_name<-preg_data_names[i]
  folder<-matched_data_folders[i]
  df_preg_dates<-fread(paste0(folder,data_name))
  print(nrow(df_preg_dates))
  
  covariate_tables<-list.files(exposure_folders[i])
  print(covariate_tables)
  print("table names")
  names_covariate<-str_sub(covariate_tables, end=-5)
  (print("variable names"))
  print(names_covariate)
  for (j in 1:length(covariate_tables)){
    exposure_data<-fread(paste0(exposure_folders[i],covariate_tables[j]))
    exposure_data$person_id<-exposure_data$id
    print(head(exposure_data))
    print(names_covariate[j])
    results<-lookback_test(expos_data = exposure_data, preg_data = df_preg_dates, lookback = (-365*2))
    print(results)
    print(j)
    filename<-paste0(names_covariate[j],"_lookback.csv")
    print(filename)
    write(results, paste0(output_folders[i],filename))
  }
}


