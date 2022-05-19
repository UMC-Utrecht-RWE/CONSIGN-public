
MATCHING_LOOP <- function(i){
  library("data.table")
   
  
  Exposed <- FILE_EXPOSED[
    sex_at_instance_creation == comb[["sex_at_instance_creation"]][i] & 
      FIRST_PFIZER == comb[["FIRST_PFIZER"]][i] & 
      YEAR_BIRTH == comb[["YEAR_BIRTH"]][i],.(person_id, month_t0,FIRST_COV_INF_2, INFP5_2, REGION)][, id := i][,t0 := comb[["FIRST_PFIZER"]][i]] 
  
  setnames(Exposed, "person_id", "Exposed")
  
  if(nrow(Exposed) == 0){
    rm(Exposed)
  }else{
    
    Controls <- FILE_CONTROL[
      sex_at_instance_creation == comb[["sex_at_instance_creation"]][i] & 
        (VAC_DATE1 > comb[["FIRST_PFIZER"]][i] | is.na(VAC_DATE1)) &
        YEAR_BIRTH %between% list(comb[["YEAR_BIRTH"]][i] - 1,  comb[["YEAR_BIRTH"]][i] + 1)
      ,.(person_id, FIRST_COV_INF)][,t0 := comb[["FIRST_PFIZER"]][i]]
    
    TEMP <- SPELLS[(comb[["FIRST_PFIZER"]][i]) >= op_start_date & comb[["FIRST_PFIZER"]][i] <= op_end_date,]
    if(any(duplicated(TEMP[["person_id"]]))){stop("More than 1 spell per subject")}
    
    ####
    Controls <- merge(x = Controls, y= TEMP, all.x = F, all.y = F , by = "person_id", allow.cartesian = F)
    setnames(Controls, "person_id", "Control")
    Controls[,month_t0 := paste0(sprintf("%02d",month(t0)),"-",year(t0))]
    
    ###
    #In the step below I merge the T0 depended information to the exposed and controls. This is an unnecesary step for the exposed because
    #this can be done before in Step_08_PrepareExposedControls. Make sure that this is done in the same manner.
    HIST <-readRDS(paste0(populations_dir,"Matching/",paste0(sprintf("%02d",month(comb[i, FIRST_PFIZER])),"-",year(comb[i, FIRST_PFIZER])),".rds"))
    Controls <- merge(x = Controls, y = HIST, by.x = c("Control"), by.y = c("person_id"), all.x = T, all.y = F, allow.cartesian = F)
    
    #I think row 79 and 80 are sufficient but then i cannot compare between the SQL method
    ###
    lapply("FIRST_COV_INF", function(x) Controls <- Controls[, paste0(eval(x),"_2") := fifelse(get(x) < t0, T, F, na = F)][,eval(x) := NULL])
    
    #Fix because it handles inaccuracy on left boarder not optimal
    lapply("INFP5", function(x) Controls <- Controls[, paste0(eval(x),"_2") := fifelse(get(x) < t0 ,T, F, na = F)][,eval(x) := NULL])
    
    rm(HIST)
    gc()
    
    scheme <- unique(Exposed[,..temp_var])
    
    for(k in 1:nrow(scheme)){
      
      
      
      x <- paste0(hist.cols2," == scheme[['",hist.cols2,"']][k]", collapse = " & ")
      
      Exposed1 <- Exposed[eval(parse(text = x)),][REGION == scheme[["REGION"]][k],]
      
      
      if(nrow(Exposed1) > 0){
        Controls1 <- Controls[eval(parse(text = x)),][REGION == scheme[["REGION"]][k],]
        
        v.exp <- Exposed1[["Exposed"]]
        if(nrow(Controls1) > 0){
          v.control <- sample(Controls1[["Control"]], length(v.exp), replace = T)
          nb_match <- rep(length(Controls1[["Control"]]), length(v.exp))
          
        }else{
          v.control <- rep(NA, length(v.exp))
          nb_match <- rep(0, length(v.exp))
          
        }
        
        
        for(y in 1:length(hist.cols2)){assign(hist.cols[y], rep(scheme[[hist.cols2[y]]][k], length(v.exp)))}
        assign("REGION", rep(scheme[["REGION"]][k], length(v.exp)))
        
        MATCHED_TEMP <- as.data.table(cbind(v.exp,v.control,  do.call(cbind, mget(hist.cols)) ,nb_match, REGION))
        setnames(MATCHED_TEMP, c("v.exp","v.control"),c("Exposed","Control"))
        
        
        
        if(nrow(MATCHED) == 0) MATCHED <- MATCHED_TEMP[0]
        
        MATCHED <- rbindlist(list(MATCHED ,MATCHED_TEMP ), use.names = T)
        
        rm(Controls1,MATCHED_TEMP)
        gc()
      } 
      rm(Exposed1, x)
      gc()
    }
    
    print(paste0(i," cycle from ", nrow(comb)," ",nrow(MATCHED)," cases matched during this cyclus (method = loop)" ))
    
    return(MATCHED)      
    
    rm(Controls,Exposed,scheme,TEMP)
    gc()
  }
  
}
