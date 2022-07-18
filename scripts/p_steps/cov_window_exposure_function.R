#detect drug dispensing dates within trimesters

# expos data is drug ATC group, many possible dispensing dates
#preg data is produced by cov_trimester function

during_cov_window<-function(atc_data, trim_data){
 
  
    # select person IDs in ATC data matching pregnancy cohort 
  atc_data<-atc_data[atc_data$person_id%in%trim_data$person_id]
    
  if(nrow(atc_data)>0){
  
    # convert atc_date to date format
    atc_data$date<- as.Date(as.character(atc_data$date),format="%Y%m%d")
    # dates--> numeric
    atc_data$num_date<-as.numeric(atc_data$date)
    #long to wide
    atc_data_wide<-dcast(setDT(atc_data), person_id ~ rowid(person_id), value.var = ("num_date"))
    #order by person_id
    atc_data_wide <- atc_data_wide[order(person_id),]  
    
    # keep only person_id also present in ATC data
  
    trim_data<-trim_data[trim_data$person_id%in%atc_data$person_id,]
    
    # extract covid+ trimesters exposed from same person_ids as exposure data (woman was exposed to drug at some point in time)
    
    #multiple pregnancies per person possible, need to reshape start and end 
    
    trim_wide<-dcast(setDT(trim_data), person_id ~ rowid(person_id), value.var = c("cov_date"))
    trim_wide<-trim_wide[order(person_id),]
    
   
    #test that person_id for exposure data and pregnancy data are the same
    if((all(trim_wide$person_id==atc_data_wide$person_id))==T){print("person_ids start match start, OK")}
    
    #test if any dispensing dates occurred within covid trimester
  
      # for each covid diagnosis (sorted by trimester) w/i eachperson compare each dispensing date...
    
    
  
  cov_30_plus<-list()
  cov_30_minus<-list()
    
  cov_trim_date<-trim_wide[,2:ncol(trim_wide)]
      
    for(i in 1:ncol(cov_trim_date)){
        my_date<-cov_trim_date[,..i]
        # date_dispensing-start_date : want values between 0 and window_days
        cov_expos_days <- as.data.frame(apply(my_date,2,function(x) atc_data_wide[,2:ncol(atc_data_wide)] - x ))
        minus30<-between(cov_expos_days, -30, 0)
        cov_30_minus[[i]]<-apply(minus30,1,any)
        plus30<-between(cov_expos_days,  0,30)
        cov_30_plus[[i]]<-apply(plus30,1,any)
      }
      
 my_results<-as.data.frame<-cbind(unlist(cov_30_minus), unlist(cov_30_plus)) 
 my_results[is.na(my_results)]<-FALSE
 colnames(my_results)<-c("cov_30_minus", "cov_30_plus")
     
  return(my_results)}else{return("no matching ATC dispensings")}
}
  
  





