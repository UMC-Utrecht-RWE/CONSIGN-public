#detect drug dispensing dates within trimesters

# expos data is drug ATC group, many possible dispensing dates
#preg data is produced by cov_trimester function

during_cov_window<-function(expos_data, cov_date){
  
  
    # select person IDs in ATC data matching pregnancy cohort +trimester
  expos_data<-expos_data[expos_data$person_id%in%trim_data$person_id]
    
    if(nrow(expos_data)>0){
      
    # convert expos_dates to numeric
    expos_data$num_date<-as.numeric(expos_data$date)
    #long to wide
    expos_data<-dcast(setDT(expos_data), person_id ~ rowid(person_id), value.var = ("num_date"))
    #order by person_id
    expos_data <- expos_data[order(person_id),]  
    
    # extract covid+ trimesters exposed from same person_ids as exposure data (woman was exposed to drug at some point in time)
    #only 1 elligible pregnancy per person, no reshaping needed
    
    
    trim_data<-trim_data %>% select(c(person_id, cov_date ))
    # convert dates to numeric
    trim_data$num_date<- as.numeric(trim_data[,cov_date])
  
   
    trim_data<-trim_data[order(person_id),]
    
    
    #test that person_id for exposure data and pregnancy data are the same
    if((all(trim_data$person_id==expos_data$person_id))==T){print("person_ids match start, OK")}else{print((trim_data$person_id==expos_data$person_id))}
  
    
    #test if any dispensing dates occurred within covid window
    
    
      
      cov_expos_diff <- as.data.frame(apply(my_start_dates,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
      
      
      
    
    my_results<-cov_expos_diff
    return(my_results)}
  else{print("no records of drug dispensing with match person_id")
    return("no trimester with this exposure")}
}




