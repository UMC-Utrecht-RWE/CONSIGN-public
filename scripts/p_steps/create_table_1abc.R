#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#21/9/2022


#This script pulls drug exposure window results from trimester_drug_exposure.R 
# and transforms the data into table 1 (a,b,c) format
################################################################
CDM_source<-fread(paste0(path_CDM,"CDM_source.csv"))
DAP<-CDM_source$data_access_provider_name

my_tables<-list.files(path=output_cov_window_atc_2)
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-22)

my_PREG<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))

denoms<-as.numeric(table(my_PREG$cov_trimester))
denom_trim_1<-denoms[1]
denom_trim_2<-denoms[2]
denom_trim_3<-denoms[3]


#1a: together
#1b" non-severe
#1c: severe

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_cov_window_atc_2,my_tables[1]))
  atc_result<-as.data.frame(during_cov_window(atc_data = my_data, trim_data = cov_trim_data))
  fwrite(atc_result, paste0(output_cov_window_atc_2,my_names[i],"_cov_window_counts.csv" ))
}


