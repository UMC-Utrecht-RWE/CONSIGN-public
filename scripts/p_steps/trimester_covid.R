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

my_PREG<-fread(paste0(pan_preg_folder,"my_PREG.csv"))

cov_data<-fread(paste0(preselect_folder,"/covid_data.csv"))


##########################################################################


#this function identifies the trimester of infection,and specifies which covid diagnosis date (in case of multiple diagnosis)

#IF multiple COVID diagnoses occured during the same pregnancy, the first diagnosis date and trimester of exposure are recorded.


cov_trim_data<-cov_trimester(preg_data = my_PREG, cov_data=cov_data)

# select only those with covid during pregnancy

cov_trim_data<-cov_trim_data[(is.na(cov_trim_data$cov_trimester))==F,]

# flag covid dates that are too close to labor to "sandbox" them in severity analysis
# there should be minimum 0 (negative days means after labor, this would be removed from data since covid_date not DURING pregnancy)

cov_trim_data$days_cov_before_labor<-(cov_trim_data$pregnancy_end_date)-(cov_trim_data$covid_date)

cov_trim_data$gest_age_cov<-(cov_trim_data$covid_date)-(cov_trim_data$pregnancy_start_date)

fwrite(cov_trim_data, paste0(pan_preg_folder,"trim_cov_PREG.csv"))

