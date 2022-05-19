#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

##Aim
#To get all the cases of Covid and put them in 1 file. Because there can be many other outcomes occur within an event table we use temporary sqlite database.
#It is going loop wise. So first grap 1 event file, than import needed variables and remove first all rows that are not needed (check row 32 in code). Than
#the codes are selected and appended in the temporary sqlite database. This is done for all the EVENT files and when the loop is completed the covid table is
#loaded back to R environment and saved as RDS.

##in/output
#Input 1: EVENTS (CDM)
#Input 2: Pfizer_full_codelist.csv
#input 3: MEDICAL_OBSERVATIONS (CDM)
#input 4: SURVEY_OBSERVATIONS (CDM)
#input 5: Pfizer_additional_MO_SO.csv
#Output 1: COVID19DX.rds



SCRIPT <- RUN_SCRIPT(name = "Step_05_GetCovidRegion.R")


#Crerate database to append concepts to
tmpdb <- dbConnect(RSQLite::SQLite(), "")


#Get codesheet
FILE <- readRDS(file = SCRIPT[["INPUT2"]][["path"]])
setnames(FILE, "event_abbreviation", "Outcome")

CDC_EVENTS <- c("DM1", "DM12")
FILE <- FILE[Outcome %in% c("COVID19DX", CDC_EVENTS),]

#FILE <- FILE[Outcome %in% CDC_EVENTS, `:=` (type = Outcome, Outcome = "CDC") ]

#Going through EVENTS tables and append to database.
####

files <- list.files(path_dir, pattern = "EVENTS")

#i=files[1]
for(i in files){
  
  TEMP <- IMPORT_PATTERN(
    dir = unlist(SCRIPT[["INPUT1"]][["folder"]]), 
    pat = i,
    colls = c("person_id", "start_date_record", "event_code", "event_record_vocabulary"),
    date.colls = "start_date_record",
    exprss = expression( person_id %in% PERSONS_OF_INTEREST1),
    append = F
  )
  
  setnames(TEMP,c("start_date_record", "event_code", "event_record_vocabulary"), c("Date", "Value", "Voc"))
  
  #Remove records before first known Covid infection
  before <- nrow(TEMP)
  TEMP <- TEMP[Date > as.Date("20200101", "%Y%m%d") & Date < end_study_date,]
  after <- nrow(TEMP)
  
  print(paste0(before - after," rows deleted from ",before," because they where before first covid infection"))
  rm(before, after)
  
  CreateConceptDatasets(
    
    codesheet = FILE,
    c.voc = "coding_system",
    c.concept = "Outcome",
    c.codes = "code",
    file = TEMP,
    f.code = "Value",
    f.voc =  "Voc",
    c.startwith = start_with_colls,
    #path = concepts_dir,
    method = "loop",
    db = tmpdb
    
  )
  
  rm(TEMP)
  gc()
  
}



rm(files, FILE)
gc() 


####


#Importing DAP specific concepts.
###
FILE <- readRDS(file = SCRIPT[["INPUT5"]][["path"]])

if(nrow(FILE) > 0){
scheme <- unique(FILE[["table"]])

needed_colls <- unique(c(colnames(FILE)[substr(colnames(FILE),1,3) == "col"], "date_column", "keep"))


#j = 2
for(j in 1:length(scheme)){

  files <- list.files(path_dir, pattern = scheme[j])
              
          FILE_TEMP <- FILE[table == scheme[j] ,]
          
      #i=files[1]
          for(i in files){
              
              TEMP <- IMPORT_PATTERN(
                dir = unlist(SCRIPT[["INPUT1"]][["folder"]]), 
                pat = i,
                colls = na.omit(unique(c("person_id", unique(do.call(rbind, lapply(needed_colls, function(x) unique(FILE_TEMP[!is.na(x)][[x]]) )))))),
                date.colls = unique(FILE_TEMP[["date_column"]]),
                exprss = expression( person_id %in% PERSONS_OF_INTEREST1),
                append = F
              )
              
              #if(!"REGION" %in% FILE_TEMP[["Outcome"]]){
              #Remove records before first known Covid infection
              #before <- nrow(TEMP)
              #TEMP <- TEMP[get(unique(FILE_TEMP[["date_column"]])) > as.Date("20200101", "%Y%m%d") & get(unique(FILE_TEMP[["date_column"]])) < end_study_date,]
              #after <- nrow(TEMP)
              
              #print(paste0(before - after," rows deleted from ",before," because they where before first covid infection"))
              #rm(before, after)
              #}
              
              
              CreateConceptDatasetsMultipleVars(
                codesheet = FILE_TEMP,
                file = TEMP,
                f.id = "person_id",
                c.columns = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "col"]),
                c.values = sort(colnames(FILE_TEMP)[substr(colnames(FILE_TEMP), 1, 3) == "val"]),
                c.date = "date_column",
                c.outcome = "Outcome",
                c.keep = "keep",
                c.keepnames = "Outcome",
                db = tmpdb 
              ) 
                
                
              
              
              
              rm(TEMP)
              gc()
              
              }



rm(files, FILE_TEMP)
gc()
              
}              
}            
rm(FILE)   
gc()


###

#Get data from temporary database
###

if(length(dbListTables(tmpdb)) > 5){stop("To many files created in database")}

if("COVID19DX" %in% dbListTables(tmpdb) &  "COVID19DX2" %in% dbListTables(tmpdb)){
  code <- "SELECT person_id, Date, Voc, Value, Outcome FROM COVID19DX UNION ALL SELECT person_id, Date, Voc, Value, Outcome FROM COVID19DX2"
  if(dbGetQuery(tmpdb, "SELECT count(*) FROM COVID19DX") == 0) print("0 cases of Covid in EVENT tables")
  if(dbGetQuery(tmpdb, "SELECT count(*) FROM COVID19DX2") == 0) print("0 cases of Covid in other tables")
  if(dbGetQuery(tmpdb, "SELECT count(*) FROM COVID19DX") == 0 & dbGetQuery(tmpdb, "SELECT count(*) FROM COVID19DX2") == 0) stop("0 cases of Covid in CDM")
  
  saveRDS(as.data.table(dbGetQuery(tmpdb, code))[, Date := as.Date(Date, origin="1970-01-01")], SCRIPT[["OUTPUT1"]][["path"]])
  rm(code)
}

if("COVID19DX" %in% dbListTables(tmpdb) &  !"COVID19DX2" %in% dbListTables(tmpdb)){
  if(dbGetQuery(tmpdb, "SELECT count(*) FROM COVID19DX") == 0) stop("0 cases of Covid in CDM")
  saveRDS(as.data.table(dbReadTable(tmpdb, "COVID19DX"))[, Date := as.Date(Date, origin="1970-01-01")], SCRIPT[["OUTPUT1"]][["path"]])
  
  
}

if(!"COVID19DX" %in% dbListTables(tmpdb) &  "COVID19DX2" %in% dbListTables(tmpdb)){
  if(dbGetQuery(tmpdb, "SELECT count(*) FROM COVID19DX2") == 0) stop("0 cases of Covid in CDM")
  saveRDS(as.data.table(dbReadTable(tmpdb, "COVID19DX2"))[, Date := as.Date(Date, origin="1970-01-01")], SCRIPT[["OUTPUT1"]][["path"]])
}

if("REGION" %in% dbListTables(tmpdb)){
  if(dbGetQuery(tmpdb, "SELECT count(*) FROM REGION") == 0) stop("0 cases of REGION in CDM")
  saveRDS(as.data.table(dbReadTable(tmpdb, "REGION"))[, Date := as.Date(Date, origin="1970-01-01")], SCRIPT[["OUTPUT2"]][["path"]])
}else{
  stop("0 cases of REGION in CDM")
}

if(any(CDC_EVENTS %in% dbListTables(tmpdb))){
  
  Query <- paste0("SELECT * FROM ", CDC_EVENTS[1],paste0(paste0(" UNION SELECT * FROM ",  CDC_EVENTS[2:length(CDC_EVENTS)]), collapse = " "))

  #if(dbGetQuery(tmpdb, "SELECT count(*) FROM CDC") == 0) stop("0 cases of CDC in CDM")
  saveRDS(as.data.table(dbGetQuery(tmpdb, Query))[, Date := as.Date(Date, origin="1970-01-01")], SCRIPT[["OUTPUT3"]][["path"]])
}else{
  stop("0 cases of CDC in CDM")
}



#dbReadTable(tmpdb, "REGION")
#dbReadTable(tmpdb, "COVID19DX")
#dbReadTable(tmpdb, "COVID19DX2")

dbListTables(tmpdb)
dbDisconnect(tmpdb)

rm(tmpdb, SCRIPT,scheme)







