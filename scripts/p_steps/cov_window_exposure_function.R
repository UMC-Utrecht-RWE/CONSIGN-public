#detect drug dispensing dates within trimesters

# expos data is drug ATC group, many possible dispensing dates
#preg data is produced by cov_trimester function

during_cov_trimester<-function(atc_data, trim_data){
 
  
    # select person IDs in ATC data matching pregnancy cohort 
  atc_data<-atc_data[atc_data$person_id%in%trim_data$person_id]
    
  if(nrow(atc_data)==0){print("no elligible drug exposures"); break}
  
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
    
    trim_start_wide<-dcast(setDT(trim_data), person_id ~ rowid(person_id), value.var = c("cov_start"))
    trim_start_wide<-trim_start_wide[order(person_id),]
    
    trim_end_wide<-dcast(setDT(trim_data), person_id ~ rowid(person_id), value.var = c("cov_end"))
    trim_end_wide<-trim_end_wide[order(person_id),]
    
    
    #test that person_id for exposure data and pregnancy data are the same
    if((all(trim_start_wide$person_id==atc_data_wide$person_id))==T){print("person_ids start match start, OK")}else{break}
    if((all(trim_end_wide$person_id==atc_data_wide$person_id))==T){print("person_ids end match start, OK")}else{break}
    
    #test if any dispensing dates occurred within covid trimester
  
      # for each covid diagnosis (sorted by trimester) w/i eachperson compare each dispensing date...
    
      cov_atc_start_diff<-list()
      
      
      for(i in 2:ncol(trim_start_wide)){
        my_start_dates<-trim_start_wide[,..i]
        cov_expos_days <- as.data.frame(apply(my_start_dates,2,function(x) atc_data_wide[,2:ncol(atc_data_wide)] - x ))
        cov_atc_start_diff[[i-1]]<-cov_expos_days
        
      }
      
      cov_atc_end_diff<-list()
      
      for(i in 2:ncol(trim_end_wide)){
        my_end_dates<-trim_end_wide[,..i]
        cov_expos_days <- as.data.frame(apply(my_end_dates,2,function(x) atc_data_wide[,2:ncol(atc_data_wide)] - x ))
        cov_atc_end_diff[[i-1]]<-cov_expos_days
        
      }
      
      # we want all rows (pregnancies) where start_diff>=0 AND end_diff<=0
      #in case of mulitple pregnancies/person there will be listed start and end differences
      # need to calculate for each list element
      
      my_result<-list()
      
      for(i in 1:length(cov_atc_start_diff)){
        start<-as.data.table(cov_atc_start_diff[[i]])
        end<-as.data.table(cov_atc_end_diff[[i]])
        
        start[]
      }
      
      (cov_atc_end_diff[[1]])[any(cov_atc_end_diff[[1]])<=0,]
     
      return(my_results)}
  
  





