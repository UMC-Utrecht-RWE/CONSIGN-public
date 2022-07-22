#simulate covid dates 
#add person_ids from ATC dataset


#also requires cov_date dataframe (person_id and date of diagnosis (cov_date))

#lastly, it pulls drug exposure results from ATC_detect 

################################################################

#test that date of drug dispensing is within trimester

my_PREG<-fread(paste0(hist_preg_folder,"my_PREG.csv"))

# simulate person_id matching drug exposure table 1
my_tables<-list.files(path=output_drugs)
my_data<-fread(paste0(output_drugs,my_tables[1]))

new_id<-sample(my_data$person_id,10)
sample<-(sample(1:nrow(my_PREG), 10,replace = T) )
sample<-my_PREG[sample,]
sample$person_id<-new_id

my_PREG<-rbind(my_PREG, sample)



#simulate covid diagnosis dates between start and end of pregnancy
(my_PREG_ <- my_PREG %>%
    rowwise() %>%
    mutate(cov_date = sample(x = seq(from = pregnancy_start_date,
                                     to = pregnancy_end_date,
                                     by = "day"),
                             size = 1)))

# simulate multiple diagnoses within person

sample2<-(sample(1:nrow(my_PREG), 5,replace = F) )
cov_dates<-my_PREG_[sample2, "cov_date"]
sample3<-(sample(1:nrow(my_PREG), 5,replace = F) )
id_data<-my_PREG_[sample3, "person_id"]

cov_data<-cbind(id_data, cov_dates)
colnames(cov_data)<-c("person_id", "cov_date")
cov_data<-as.data.frame(rbind(cov_data, my_PREG_[,c("person_id", "cov_date")]))
severity<-sample(c("severe", "nonsevere"), nrow(cov_data), replace = T)
cov_data$severity<-severity

my_PREG$pregnancy_id<-1:nrow(my_PREG)



fwrite(my_PREG, paste0(pan_preg_folder,"sim_PREG.csv") )
fwrite(cov_data, paste0(pan_preg_folder,"sim_cov.csv") )

my_PREG<-fread(paste0(pan_preg_folder,"sim_PREG.csv") )
cov_data<-fread(paste0(pan_preg_folder,"sim_cov.csv") )
 

rm(cov_dates)
rm(id_data)
rm(my_PREG_)
rm(sample)
rm(my_data)
