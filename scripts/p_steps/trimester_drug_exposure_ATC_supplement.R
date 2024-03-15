#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#13/7/2022


#This script pulls drug exposure results from ATC_detect and tests if any of 
#the dispensing dates occur within the trimester of covid diagnosis

################################################################



#this function identifies the trimester of infection, calculates the covid window + or- 30 days from diagnosis, 
#and subsets the data by trimester of diagnosis
#output is a list of 3 dataframes (trim1, trim2, trim3) with the diagnosis date, covid window dates and person_ids


cov_trim_data<-fread(paste0(pan_preg_folder,"covid_positive/cov_pos_preg.csv"))

my_paths<-c(raw_atc_3_counts, raw_atc_4_counts, raw_atc_5_counts)
output_paths<-c(output_cov_window_atc_3, output_cov_window_atc_4, output_cov_window_atc_5)
for(j in 1:length(my_paths)){

my_tables<-list.files(path=my_paths[j])
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-4)


# now I need to test the dates of dispensing by person against each pregnancy trimester of covid diagnosis


for(i in 1:length(my_tables)){
 my_data<-fread(paste0(my_paths[j],my_tables[i]))
  atc_result<-as.data.frame(during_cov_window(atc_data = my_data, trim_data = cov_trim_data))
  fwrite(atc_result, paste0(output_paths[j],my_names[i],"_cov_window_counts.csv" ))
}
}
# the result for table 1 group are the sums of each column