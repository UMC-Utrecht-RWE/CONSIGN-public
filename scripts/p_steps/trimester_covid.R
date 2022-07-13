#set look_back parameter#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#29/6/2022

# this script requires PREG_data which has been processed using the trimester function in the pregnancy_filter script which creates the cohorts
# should be run on the pan_preg (pregnant during the covid 19 pandemic) cohort

#also requires cov_date dataframe (person_id and date of diagnosis (cov_date))

#lastly, it pulls drug exposure results from ATC_detect 

################################################################

#test that date of drug dispensing is within trimester

my_PREG<-fread(paste0(pan_preg_folder,"sim_PREG.csv"))

cov_data<-fread(paste0(pan_preg_folder,"sim_cov.csv"))
##########################################################################


#this function identifies the trimester of infection, calculates the covid window + or- 30 days from diagnosis, 
#and subsets the data by trimester of diagnosis
#output is a list of 3 dataframes (trim1, trim2, trim3) with the diagnosis date, covid window dates and person_ids

cov_trim_data<-cov_trimester(preg_data = my_PREG, cov_data=cov_data)
# cov_trim_data<-fread(paste0(pan_preg_folder,"trim_cov_PREG_sim.csv"))

cov_trim_data<-cov_trim_data[(is.na(cov_trim_data$cov_trimester))==F,]

cov_trim_data$cov_start<-NA

cov_trim_data$cov_start[cov_trim_data$cov_trimester==1]<-cov_trim_data$trim_1_start[cov_trim_data$cov_trimester==1]
cov_trim_data$cov_start[cov_trim_data$cov_trimester==2]<-cov_trim_data$trim_2_start[cov_trim_data$cov_trimester==2]
cov_trim_data$cov_start[cov_trim_data$cov_trimester==3]<-cov_trim_data$trim_3_start[cov_trim_data$cov_trimester==3]

cov_trim_data$cov_end<-NA
cov_trim_data$cov_end[cov_trim_data$cov_trimester==1]<-cov_trim_data$trim_1_end[cov_trim_data$cov_trimester==1]
cov_trim_data$cov_end[cov_trim_data$cov_trimester==2]<-cov_trim_data$trim_2_end[cov_trim_data$cov_trimester==2]
cov_trim_data$cov_end[cov_trim_data$cov_trimester==3]<-cov_trim_data$trim_3_end[cov_trim_data$cov_trimester==3]

fwrite(cov_trim_data, paste0(pan_preg_folder,"trim_cov_PREG.csv"))
