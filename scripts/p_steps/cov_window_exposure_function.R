#detect drug dispensing dates within trimesters

# expos data is drug ATC group, many possible dispensing dates
#preg data is produced by cov_trimester function

during_cov_window<-function(expos_data, cov_data){
 
  
    # select person IDs in ATC data matching pregnancy cohort +trimester
  expos_data<-expos_data[expos_data$person_id%in%cov_data$person_id]
    
  if(nrow(expos_data=0)){break
    print("no elligible drug exposures")}
  
    # convert expos_dates to numeric
    expos_data$num_date<-as.numeric(expos_data$date)
    #long to wide
    expos_data<-dcast(setDT(expos_data), person_id ~ rowid(person_id), value.var = ("num_date"))
    #order by person_id
    expos_data <- expos_data[order(person_id),]  
    
    # extract covid+ trimesters exposed from same person_ids as exposure data (woman was exposed to drug at some point in time)
    #only 1 elligible pregnancy per person, no reshaping needed
  
    
    cov_data$num_date<- as.numeric(cov_data[,cov_date])
    cov_data<-dcast(setDT(cov_data), person_id ~ rowid(person_id), value.var = ("num_date"))
  
   
    cov_data<-cov_data[order(person_id),]
    
    
    #test that person_id for exposure data and pregnancy data are the same
    if((all(cov_data$person_id==expos_data$person_id))==T){print("person_ids match start, OK")}else{print((cov_data$person_id==expos_data$person_id))}
  
    
    #test if any dispensing dates occurred within covid window
    
    
      
      # cov_expos_diff <- as.data.frame(apply(cov_data$num_date,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
      
      # for each covid diagnosis (sorted by trimester) w/i eachperson compare each dispensing date...
    
      cov_expos_diff<-list()
      
      
      for(i in 2:ncol(cov_date)){
        my_start_dates<-cov_date[,..i]
        cov_expos_days <- as.data.frame(apply(cov_data$num_date,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
        cov_expos_diff[[i-1]]<-cov_expos_days
        
      }
      
      my_results<-cbind(cov_data$person_id, as.data.frame(unlist(cov_expos_diff)))
      cov_num<-vector()
      for(i in 1:ncol(alg_result)){
        cov_num[i]<-paste0("cov_diagnosis_",i)
      }
      colnames(my_results)<-c("person_id",cov_num )
      return(my_results)}
  
  





