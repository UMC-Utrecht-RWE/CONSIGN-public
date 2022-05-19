



GetDatesIR <- function(AESI, Start_date, FILE, c.name){
  
  library(data.table)
  library(RSQLite)
  library(DBI)
  #########
  

    mydb <- dbConnect(RSQLite::SQLite(), paste0(tmp,"database.db"))

    temp1 <- as.data.table(dbGetQuery(mydb,paste0(
     "

     SELECT person_id,Date, id  FROM(
     SELECT
     * ,
     ROW_NUMBER () OVER (
                     PARTITION BY person_id, id
                     ORDER BY Date ASC
                     )  NB


     FROM(

     SELECT DISTINCT
     t1.person_id,
     t1.id,
     t2.Date

     FROM ",FILE," t1

     inner join ",AESI," t2

     ON (t1.person_id = t2.person_id AND (t2.Date BETWEEN (t1.",Start_date," - 0) AND t1.op_end_date))

 	  )
    )
   WHERE NB = 1

     "

   )
   )
   )

   temp2 <- as.data.table(dbGetQuery(mydb,paste0(
     "

     SELECT person_id,Date, id  FROM(
     SELECT
     * ,
     ROW_NUMBER () OVER (
                     PARTITION BY person_id, id
                     ORDER BY Date DESC
                     )  NB

     FROM(

     SELECT DISTINCT
     t1.person_id,
     t1.id,
     t2.Date

     FROM ",FILE,"  t1

     inner join ",AESI," t2

     ON (t1.person_id = t2.person_id AND (t2.Date BETWEEN (t1.",Start_date," - (5*365.25)) AND (t1.",Start_date," - 1)))

 	  )
   )
   WHERE NB = 1

    "

   )
   )
   )



  temp <- list(file1 = temp1, file2 = temp2, AESI = c.name)
  
  print(paste0("Dates for column ",c.name," are collected"))
  
  return(temp)
  dbDisconnect(mydb)

  rm(temp,temp1,temp2)
  
  gc()
}
  
