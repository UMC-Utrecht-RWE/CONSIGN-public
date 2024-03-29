#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#29/6/2022

# this function identifies the trimester when COVID diagnosis occurred. 
#inputs: 
  #pregnancy dataset with minimum person_id and trimester dates (generated by trimester function)
  # cov_date: dataset with person_id and date of diagnosis (labeled cov_date)

cov_trimester<-function(preg_data=my_PREG, cov_data=covid_data){


#compare my_PREG for each covid diagnosis per person per pregnancy
  # 1) match person_ids
  # 2) my_PREG long to wide by preg_ID, select trim variables
  # 3) cov_data long to wide by date
  # 4) for each row check each covid date (in case of multiple diagnoses) to each pregnancy dates (in case of multiple pregnancies)

  # inner join?
  
PREG_long<- preg_data[preg_data$person_id%in%cov_data$person_id,] 
PREG_long<-PREG_long[order(PREG_long$person_id),]


PREG_wide<-dcast(setDT(PREG_long), person_id ~ rowid(person_id), value.var = ("pregnancy_id"))
PREG_wide<-PREG_wide[order(PREG_wide$person_id),]

if(nrow(PREG_wide)>0){print("pregnancy data OK")}else{print("pregnancy data empty")}

preg_names<-vector()
for(i in 2:ncol(PREG_wide)){
  preg_names[i-1]<-paste0("pregnancy_",(i-1))
}


colnames(PREG_wide)<-c("person_id",preg_names)


cov_data<-cov_data[cov_data$person_id%in%PREG_wide$person_id,]
# make dates from lowest (first, oldest record) to highest (most recent)
cov_data<-cov_data[order(cov_data$cov_date),]

max_cov<-max(table(cov_data$person_id))
cov_data_wide<-dcast(setDT(cov_data), person_id ~ rowid(person_id), value.var = c("cov_date"))
cov_data_wide<-cov_data_wide[order(cov_data_wide$person_id),]

if(nrow(cov_data_wide)>0){print("covid data OK")}else{print("covid data empty")}

sev_data_wide<-dcast(setDT(cov_data), person_id ~ rowid(person_id), value.var = c("severe"))
sev_data_wide<-sev_data_wide[order(sev_data_wide$person_id),]

if(nrow(sev_data_wide)>0){print("severity data OK")}else{print("severity data empty")}

if((all(cov_data_wide$person_id==PREG_wide$person_id))==F){print("person_id match failure");break}else{print("id match OK")}


cov_num<-vector()
for(i in 1:max_cov){
  cov_num[i]<-paste0("cov_date_", i)
}

colnames(cov_data_wide)<-c("person_id",cov_num)

sev_num<-vector()
for(i in 1:max_cov){
  sev_num[i]<-paste0("severity_", i)
}

colnames(sev_data_wide)<-c("person_id",sev_num)

cov_PREG<-cbind(PREG_wide, cov_data_wide[,2:ncol(cov_data_wide)], sev_data_wide[,2:ncol(sev_data_wide)])

print(cov_PREG)

# now I have the covid dates matched to the right person_id and per pregnancy in wide format by PREG_ID- 
# now I need it back to long to cbind to preg_long to calculate the trimester (if any) in which diagnosis occurred
# problem: variable column number and column names

interim_cov_long<-melt.data.table(cov_PREG, id.vars = c("person_id", cov_num, sev_num),measure.vars = preg_names, na.rm = T)

order_cov_long<-interim_cov_long[order(interim_cov_long$person_id),]

if(nrow(order_cov_long)>0){print("covid dates matching DF OK")}else{print("covid dates matching DF empty")}

#now I have each covid diagnosis date repeated once per pregnancy. Long by pregnancy, wide by covid

if((all(order_cov_long$person_id==PREG_long$person_id))==F){print("person_id match failure");break}else{print("id match OK")}

cov_final<-select(order_cov_long,starts_with("cov_date"))
sev_final<-select(order_cov_long,starts_with("sev"))

#problem to think about: if a person has more than one diagnosis, the second could overwrite the first
#example: first diagnosis is in 2nd trimester (cov_trimester==2), second diagnosis is after pregnancy (cov_trimester==NA)
#need to add something to allow NA and 0 to be overwritten but not 1,2,3

#define final_PREG_wide in the beginning of the loop to remove those already assigned a trimester
#what if a single pregnancy has 2 infections? HELP (take first? since medicines exposure most important in first)?
#5/7/22 eimir : take first covid exposure

#HELP 5/7/22 covid diagnosis just before LMP (lookback 10 days? quarantine days)--> NO (eimir)

# 15/7: need to retain the specific cov_date that matches the trimester, then the cov_window== +30 days and -30 day separately >_<



PREG_long$cov_trimester<-NA
PREG_long$covid_date<-NA
PREG_long$severity<-NA

date_match<-as.data.frame(matrix(ncol=ncol(cov_final), nrow=nrow(cov_final)))
colnames(date_match)<-colnames(cov_final)
for(i in 1:ncol(cov_final)){

 cov_date<-cov_final[,..i]
 
# is cov_date in trimester 1 window?
PREG_long$cov_trimester[is.na(PREG_long$cov_trimester) & (cov_date>=as.numeric(PREG_long$trim_1_start))&  (cov_date<= as.numeric(PREG_long$trim_1_end))]<-1

# is cov_date in trimester 2 window?
PREG_long$cov_trimester[is.na(PREG_long$cov_trimester) & (cov_date>=as.numeric(PREG_long$trim_2_start))&  (cov_date<= as.numeric(PREG_long$trim_2_end))]<-2

# is cov_date in trimester 3 window?
PREG_long$cov_trimester[is.na(PREG_long$cov_trimester) & (cov_date>=as.numeric(PREG_long$trim_3_start))&  (cov_date<= as.numeric(PREG_long$trim_3_end))]<-3

# did the cov_date from this column match?
date_match[,i]<-(is.na(PREG_long$cov_trimester)==F)


}


date_match_num<-sapply(date_match,as.numeric)
date_match_all<-date_match_num*cov_final
#remove 0s to find lowest, non-0 date 
date_match_all[date_match_all==0]<-NA
my_min<-function(x) {ifelse( !all(is.na(x)), min(x, na.rm=T), NA)}
date_match_min<-apply(date_match_all, 1, FUN = my_min)
PREG_long$covid_date<-date_match_min

#severity
severity_match<-as.matrix(sev_final)*as.matrix(date_match_all)
sev_match_filter<-apply(severity_match, 1, FUN = my_min)
sev_match_filter[sev_match_filter>0]<-1

PREG_long$severity<-sev_match_filter

return(PREG_long)
}

