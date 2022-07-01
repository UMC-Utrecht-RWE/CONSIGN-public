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

my_PREG<-fread(paste0(hist_preg_folder,"my_PREG.csv"))

# simulate person_id matching drug exposure table 1

new_id<-sample(my_data$person_id,10)
sample<-(sample(1:17, 10,replace = T) )
sample<-my_PREG[sample,]
sample$person_id<-new_id

my_PREG<-rbind(my_PREG, sample)

#simulate covid diagnosis dates between start and end of pregnancy
(my_PREG <- my_PREG %>%
    rowwise() %>%
    mutate(cov_date = sample(x = seq(from = pregnancy_start_date,
                                        to = pregnancy_end_date,
                                        by = "day"),
                                size = 1)))

# simulate multiple diagnoses within person

sample2<-(sample(1:nrow(my_PREG), 15,replace = F) )
cov_data<-my_PREG[sample2, c("person_id","cov_date")]
colnames(cov_data)<-c("person_id", "cov_date")



##########################################################################


#this function identifies the trimester of infection, calculates the covid window + or- 30 days from diagnosis, 
#and subsets the data by trimester of diagnosis
#output is a list of 3 dataframes (trim1, trim2, trim3) with the diagnosis date, covid window dates and person_ids

cov_trim_data<-cov_trimester(preg_data = my_PREG, cov_data=cov_data)

my_tables<-list.files(path=output_drugs)
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-4)


# my_results<-list()
# for(j in 1:length(cov_data)){
#   my_cov_data<-cov_data[[j]]
# for(i in length(my_tables)){
 my_data<-fread(paste0(output_drugs,my_tables[1]))
# my_results[i]<-
# 
# }

  during_cov_window(expos_data = my_data, cov_data = as.data.frame(cov_data[[1]]))
  
