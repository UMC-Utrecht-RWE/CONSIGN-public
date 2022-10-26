#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#26/10/2022

# covid covariates lookback

exposure_folders<-c(output_cov_neg_pan_preg, output_cov_pos_non_preg, output_cov_pos_pan_preg)

matched_data_folders<-c(matched_folder, matched_folder, cov_pos_pan_preg_folder)

preg_data_names<-c("matches_pregnant_cov_neg.csv", "matches_cov_pos_non_preg.csv", "cov_pos_preg.csv")

output_folders<-c(output_cov_pregnant_control, output_cov_nonpregnant_control,output_cov_cases)

for(i in 1:length(exposure_folders)){
  data_name<-preg_data_names[i]
  folder<-matched_data_folders[i]
  df_preg_dates<-fread(paste0(folder,data_name))
  # print(nrow(df_preg_dates))
  
  covariate_tables<-list.files(exposure_folders[i])
  # print(covariate_tables)
  names_covariate<-str_sub(covariate_tables, end=-5)
  # print(names_covariate)
  for (j in 1:length(covariate_tables)){
    exposure_data<-fread(paste0(exposure_folders[i],covariate_tables[j]))
    exposure_data$person_id<-exposure_data$id
    # print(head(exposure_data))
    # print(names_covariate[j])
    results<-lookback_test(expos_data = exposure_data, preg_data = df_preg_dates, lookback = (-365))
    # print(results)
    write(results, paste0(output_folders[i],names_covariate[j],"_lookback.csv"))
  }
}


