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
    
    trim_wide<-dcast(setDT(trim_data), person_id ~ rowid(person_id), value.var = c("cov_date", "cov_trimester", "severity"))
    trim_wide<-trim_wide[order(person_id),]
    
   
    #test that person_id for exposure data and pregnancy data are the same
    if((all(trim_wide$person_id==atc_data_wide$person_id))==T){print("person_ids start match start, OK")}
    
    #test if any dispensing dates occurred within covid trimester
  
      # for each covid diagnosis (sorted by trimester) w/i eachperson compare each dispensing date...
    
   
    
  #if there are more than 1 covid+ trimesters within same person, there will be multiple cov_date columns, 
  # each needs to be compared to the ATC dates per person
  
  
  cov_trim_date<-select(trim_wide,starts_with("cov_date"))
  severity<-select(trim_wide,starts_with("sev"))
  # IN CASE THERE ARE MULTIPLE ROWS OF SEVERITY, THERE ARE "BLANK" VALUES FOR THOSE WITHOUT A SECOND COVID+TRIMESTER
  #THIS CAUSES PROBLEMS
  # severity <- severity[severity==""]
  length(unlist(severity, use.names = F))
  trimester<-select(trim_wide,starts_with("cov_trim"))
  
  
  # store matrix output in list, one item per max(covid+ trimester)/person_id
  
  cov_30_plus_minus<-list()
 
      
    for(i in 1:ncol(cov_trim_date)){
        my_date<-cov_trim_date[,..i]
        # date_dispensing-start_date : want values between 0 and window_days
        cov_expos_days <- as.data.frame(apply(my_date,2,function(x) atc_data_wide[,2:ncol(atc_data_wide)] - x ))
        
        minus30<-between(cov_expos_days, -30, 0)
        plus30<-between(cov_expos_days,  0,30)
        
        cov_window_df<-as.data.frame(cbind(trim_wide$person_id, apply(minus30,1,any),apply(plus30,1,any), my_date, severity[,..i], trimester[,..i]))
        
        colnames(cov_window_df)<-c("person_id", "minus_30", "plus_30", "cov_date", "severity", "cov_trimester")
        cov_30_plus_minus[[i]]<- cov_window_df
        }
      
     my_results<-bind_rows(cov_30_plus_minus)
  return(my_results)}else{return("no matching ATC dispensings")}
}
  
  





