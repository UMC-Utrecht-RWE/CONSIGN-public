
#' Package ConceptionCDM
#'
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name CreateConceptDatasets
#' @keywords ??
#' @import data.table Rcpp sqldf rlist

NULL


#' @param codesheet data.table file with the codes, concept and coding system
#' @param file CDM table with codes
#' @param c.voc codesheet coding system column name
#' @param c.concept codesheet concept column name
#' @param c.codes codesheet code column name
#' @param coding systems that merges based on start with
#' @param f.code file code column name
#' @param f.voc file coding system column name
#' @param path location to write the RDS files to per concept name
#' @param method if SQL, a join is used. If loop a loop is used to subset

#' @return RDS files to per concept name with the rows that have the code specified in the codesheet

#' @export


CreateConceptDatasets <- function(codesheet=covid_codelist,file=events_tables, c.voc="coding_system", c.concept="event_abbreviation", c.codes="code", c.startwith = c("ICD9CM", "ICD9",  "ICD10", "ICD10CM", "ICD10DA"),
                                  f.code="event_code", f.voc="event_record_vocabulary", path = preselect_folder, method = "loop", group = T, f.name = NULL, db = NULL ){
  # browser()
  #Get data. If not use copy input dataset may be affected (see data.table properties)
  codesheet <- copy(codesheet)
  file <- copy(file)
  
  #Check if vocabulary matches the feeded codesheet coding system.
  systems_data <- unique(na.omit(file[[f.voc]]))
  systems_codesheet <- unique(na.omit(codesheet[[c.voc]]))
  systems_analysed <- systems_data[systems_data %in% systems_codesheet]
  systems_nanalysed <- systems_data[!systems_data %in% systems_codesheet]
  if(length(systems_nanalysed) > 0) print(paste0(systems_nanalysed," is not in codesheet so these rows are lost."))
  
  if(length(systems_analysed) == 0){print("No concepts created because there are no matching vocabularies")}else{
    
    #check if specified c.startwith is in codesheet
    if(sum(!c.startwith %in% systems_codesheet > 0)) print("check c.startwith, misspelling occured resulting in missing codes")
    
    #Standardize columns names. This to prevent the use of the get, eval statements which complicate code readability and give sometimes complex errors
    setnames(codesheet,c(c.voc,c.concept,c.codes),c("Type","Concept","Code"))
    setnames(file,c(f.voc,f.code),c("Type2","Code2"))
    
    #Create variable code_no_dot by removing dot from all codes
    codesheet[,code_no_dot := gsub("\\.","",codesheet[,Code])]
    file[,code_no_dot2 := gsub("\\.","",file[,Code2])]
    
    #codesheet[,dot_present := str_detect(codesheet[,Code],"\\.")]
    
    #Merge codesheet to data file using sql. Then it is possible to merge on start_with or exact in 1 step
    if(method == "SQL"){
      
      codesheet[,start_with := fifelse(substr(Code,nchar(Code),nchar(Code) + 1) == "." | Type %in% c.startwith ,"T","F")]
      
      TEMP1 <- sqldf("
          select distinct t1.*, t2.Concept
          from file t1
          inner join codesheet t2
          on (
                t1.Type2 = t2.Type
                and
                (
                      (
                      t2.start_with = 'T'
                      and
                      substr(t1.code_no_dot2,1,length(t2.code_no_dot)) = substr(t2.code_no_dot,1,length(t2.code_no_dot))
                      )
                      or
                      (
                      t2.start_with = 'F'
                      and
                      t1.code_no_dot2 = t2.code_no_dot
                      )
                )
              )
          ")
      
      TEMP1 <- as.data.table(TEMP1)
      
      #TEMP1[,start_with := NULL]
      setorder(TEMP1, person_id, Concept,Type2,Code2, code_no_dot2)
      setnames(TEMP1,c("Type2","Code2","code_no_dot2","Concept"),c(f.voc,f.code,paste0(f.code,"_2"),c.concept))
      
      if(group){
        for(i in unique(codesheet[["Concept"]])){
          TEMP2 <- TEMP1[get(c.concept) == i,]
          if(is.null(db)) saveRDS(object = TEMP2, file = paste0(path,"/",i,".rds"))
          
          if(!is.null(db)){
            dbWriteTable(db, i ,TEMP2, overwrite = F, append = T) 
          }
          
          rm(TEMP2)
          gc()
          
        }
      }else{
        if(is.null(db)) saveRDS(object = TEMP1, file = paste0(path,"/",f.name,".rds"))
        
        if(!is.null(db)){
          dbWriteTable(db, f.name ,TEMP1, overwrite = F, append = T) 
        }
        
        
      }
      
      rm(TEMP1)
      gc()
      
    }
    
    
    
    if(method == "loop"){
      
      #Create variable to distinct codes that need to be extracted based on start with and not exact.
      codesheet[,start_with := fifelse(substr(Code,nchar(Code),nchar(Code) + 1) == "." | Type %in% c.startwith ,T,F)]
      
      # Create list for the for loop
      #First 1 list with codes for exact matching
      conditions1 <- vector(mode="list", length = length(unique(na.omit(codesheet[["Concept"]]))))
      names(conditions1) <- unique(na.omit(codesheet[["Concept"]]))
      
      for (i in 1:length(conditions1)){
        vocabularies <- vector(mode="list", length = length(systems_analysed))
        names(vocabularies)<-systems_analysed
        for (j in 1:length(vocabularies)){
          vocabularies[[j]]$exact <- codesheet[Concept == names(conditions1)[i] & Type == names(vocabularies)[j] & start_with == F, code_no_dot]
          vocabularies[[j]]$start <- codesheet[Concept == names(conditions1)[i] & Type == names(vocabularies)[j] & start_with == T, code_no_dot]
        }
        conditions1[[i]]<-list.append(conditions1[[i]],vocabularies)
        rm(vocabularies)
      }
      #remove empty vocabularies
      conditions1<-lapply(conditions1, function(x) Filter(length, x))
      
      
      #Write codes to disk per concepts
      
      for(i in 1:length(names(conditions1))){
        
        TEMP <- file[0]
        
        for(j in 1:length(names(conditions1[[i]]))){
          TEMP2 <- copy(file)
          TEMP2 <- unique(TEMP2[
            
            Type2 == names(conditions1[[i]][j])
            &
              (
                length(conditions1[[i]][[j]]$start) > 0 & grepl(pattern =  paste0("^",conditions1[[i]][[j]]$start,collapse ="|"), x = code_no_dot2)
                |
                  length(conditions1[[i]][[j]]$exact) > 0 & code_no_dot2 %in% conditions1[[i]][[j]]$exact
              )
            
            ,])
          
          TEMP <- rbindlist(list(TEMP,TEMP2),fill = T, use.names = T)
          rm(TEMP2)
        }
        
        TEMP <- TEMP[, Concept := names(conditions1)[[i]]]
        #TEMP[,start_with := NULL]
        setorder(TEMP, person_id, Concept,Type2,Code2, code_no_dot2)
        setnames(TEMP,c("Type2","Code2","code_no_dot2","Concept"),c(f.voc,f.code,paste0(f.code,"_2"),c.concept))
        
        if(is.null(db)) saveRDS(object = TEMP, file = paste0(path,"/",names(conditions1)[[i]],".rds"))
        if(!is.null(db)){
          dbWriteTable(db, names(conditions1)[[i]] ,TEMP, overwrite = F, append = T) 
        }
        
        rm(TEMP)
        gc()
      }
      
    }
    
  } 
}
