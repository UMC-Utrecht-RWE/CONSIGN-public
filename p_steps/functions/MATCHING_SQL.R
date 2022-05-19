MATCHING_SQL <- function(i){
  
  
  library("data.table")
  library("RSQLite")
  library("DBI")
  
  

  Exposed <- FILE_EXPOSED[month_t0 == i,]
  
  HIST <-readRDS(paste0(populations_dir,"Matching/",i,".rds"))
  Controls <- merge(x = FILE_CONTROL, y = HIST, by = "person_id", all.x = T, all.y = F, allow.cartesian = F)  
  Controls <- Controls[is.na(INFP5), INFP5 := end_study_date ][is.na(FIRST_COV_INF), FIRST_COV_INF := end_study_date ]
  
  mydb <- dbConnect(RSQLite::SQLite(), "")
  
  dbWriteTable(mydb, "Exposed", Exposed)
  dbWriteTable(mydb, "Controls", Controls)
  dbWriteTable(mydb, "SPELLS", SPELLS)
  dbListTables(mydb)
  
  p <- dbSendStatement(mydb, "CREATE INDEX Exposed_index ON Exposed(person_id, region, FIRST_PFIZER, YEAR_BIRTH, FIRST_COV_INF_2, INFP5_2,FIRST_COV_INF, INFP5, sex_at_instance_creation)")
  dbClearResult(p)
  
  p <- dbSendStatement(mydb, "CREATE INDEX Controls_index ON Controls(VAC_DATE1,person_id, region, YEAR_BIRTH, FIRST_COV_INF, INFP5, sex_at_instance_creation)")
  dbClearResult(p)
  
  
  p <- dbSendStatement(mydb, "CREATE INDEX SPELLS_index ON SPELLS(person_id, op_start_date, op_end_date)")
  dbClearResult(p)
  
  
  
  p <- dbSendStatement(mydb,
                       "                     
            CREATE TABLE TEMP as                  
            
            SELECT DISTINCT
            t1.*,
            t2.op_start_date,
            t2.op_end_date
            
            FROM (
              
              SELECT DISTINCT
              t1.person_id as Exposed,
              t1.FIRST_PFIZER as T0,
              t1.FIRST_COV_INF_2,
              t1.INFP5_2,
              t1.region,
              t1.sex_at_instance_creation,
              t1.YEAR_BIRTH,
              t2.person_id as Control
              
              
              from (SELECT * FROM Exposed ) t1 
              
              inner join Controls t2
              
              on(
                
                (t1.sex_at_instance_creation = t2.sex_at_instance_creation) 
                
                AND
                
                (t1.FIRST_PFIZER < t2.VAC_DATE1 OR t2.VAC_DATE1 IS NULL)
                
                
                AND
                
                (t1.YEAR_BIRTH BETWEEN t2.YEAR_BIRTH - 1 AND t2.YEAR_BIRTH + 1 )
                
                AND
                
                ((t1.INFP5 < t1.FIRST_PFIZER AND t2.INFP5 < t1.FIRST_PFIZER) OR (t1.INFP5 >= t1.FIRST_PFIZER AND t2.INFP5 >= t1.FIRST_PFIZER))
                
                
                AND
                
                ((t1.FIRST_COV_INF < t1.FIRST_PFIZER AND t2.FIRST_COV_INF < t1.FIRST_PFIZER) OR (t1.FIRST_COV_INF >= t1.FIRST_PFIZER AND t2.FIRST_COV_INF >= t1.FIRST_PFIZER))
                
                AND 
                
                t1.region = t2.region
                
              )
            ) t1 
              
              inner join SPELLS t2 on (t1.Control = t2.person_id AND T0 BETWEEN t2.op_start_date AND (t2.op_end_date - 0))
              
              
            
           "
  )
  
  dbClearResult(p)
  
  TEMP1 <- dbGetQuery(mydb,
                      
                      "
          SELECT * FROM(
          SELECT * , ROW_NUMBER () OVER ( 
                  PARTITION BY Exposed
                  ORDER BY Exposed, random()
          		
              ) NB  
          
          FROM TEMP
          )
          
          WHERE NB = 1
          
          "                    
                      
  )
  
  TEMP2 <- dbGetQuery(mydb, "SELECT Exposed, COUNT(Control) as nb_match FROM TEMP GROUP BY Exposed")
  
  TEMP <- merge(TEMP1, TEMP2, by = "Exposed") 
  
  
  
  dbDisconnect(mydb)
  
  print(paste0("month ",i," is matched (method = SQL)"))
  return(as.data.table(TEMP))
  
  
  rm(TEMP,TEMP1,TEMP2)
  gc()
  
  
}
