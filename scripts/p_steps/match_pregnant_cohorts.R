# Matching instructions
# These person_ids can be matched using the pregnancy tables, variables are 
#  1)pregnancy_start_date   
#  2)age_group 
#
#These need to be matched by cov_date  (and age? age_at_cov_date? Eimir) (age_at_cov_date needs to be calculated)

#Set pipeline project directory with path to source code folder 
# to be used by all other folder definition
#projectDir<-dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(projectDir)

#Set source folder for data sources 
# based on projectDir, for both populations
dataPregPosDir<-paste0(projectFolder,"/CDMInstances_pan_pregnant/covid_positive/")  
dataNotPregDir<-paste0(projectFolder,"/CDMInstances_not_pregnant/covid_positive/")  
dataPregNegDir<-paste0(projectFolder,"/CDMInstances_pan_pregnant/covid_negative/")  

# read exposed file
#t1 <- read.csv(paste0(dataPregPosDir,"cov_pos_preg.csv"))

t1_all <- read.csv(paste0(dataPregPosDir,"cov_pos_preg.csv"))
t1_grouped<-t1_all%>%group_by(person_id)
t1<-t1_grouped%>%slice_min(n = 1, order_by = pregnancy_start_date)


# # read control file
# t2 <- read.csv(paste0(dataNotPregDir,"PERSONS.csv"))
# read control file

# adding line to select cov_neg_pan_preg to take FIRST pregnancy so we don't select same mother twice as control

t2_all <- read.csv(paste0(dataPregNegDir,"cov_neg_preg.csv"))
t2_grouped<-t2_all%>%group_by(person_id)


t2<-t2_grouped%>%slice_min(n = 1, order_by = pregnancy_start_date)


# look at first three lines to test
sqldf("select * from t1 limit 3")
# sqldf("select * from t2 limit 3")
sqldf("select * from t2 limit 3")

# 111111111111111111111111111111111111111111111111111111111
# execute matching: 1st round 
# 11111111111111111111111111111111111111111111111111111111

round1p <-sqldf(

"WITH 
gt1 AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0) ORDER BY person_id) AS a_row
  FROM t1), 
gt2 AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0)  ORDER BY person_id) AS b_row
  FROM t2
)
SELECT 
       gt1.person_id exposed_id,
       gt2.person_id control1_id,
       gt2.age_group,
       gt2.pregnancy_start_date
FROM gt1
LEFT JOIN gt2
ON gt1.age_group = gt2.age_group
   AND round(gt1.pregnancy_start_date/28,0) = round(gt2.pregnancy_start_date/28,0) 
   AND a_row = b_row
ORDER BY gt2.person_id", dbname = "consign.db")


#22222222222222222222222222222222222222222222222222222222
# execute matching: 2st round 
#22222222222222222222222222222222222222222222222222222222
round2p <-sqldf(
  
  "WITH 
gt1 AS (
  SELECT
  person_id ,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0)  ORDER BY person_id) AS a_row
  FROM t1), 

gt2 AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0)  ORDER BY person_id) AS b_row
  FROM t2
  WHERE t2.person_id NOT IN (select control1_id from round1p)

)
SELECT 
gt1.person_id exposed_id,
gt2.person_id control2_id,
gt2.age_group,
gt2.pregnancy_start_date
FROM gt1
LEFT JOIN gt2
ON gt1.age_group = gt2.age_group
AND round(gt1.pregnancy_start_date/28,0) = round(gt2.pregnancy_start_date/28,0) 

AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign.db", verbose=TRUE)



#333333333333333333333333333333333333333333333333333333333333333333333333
# execute matching: 3st round 
#333333333333333333333333333333333333333333333333333333333333333333333333
round3p <- sqldf(
  
  "WITH 
gt1 AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, pregnancy_start_date ORDER BY person_id) AS a_row
  FROM t1), 

gt2 AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, pregnancy_start_date ORDER BY person_id) AS b_row
  FROM t2
  WHERE t2.person_id NOT IN (select control1_id from round1p)
  AND t2.person_id NOT IN (select control2_id from round2p)
)
SELECT 
gt1.person_id exposed_id,
gt2.person_id control3_id,
gt2.age_group,
gt2.pregnancy_start_date
FROM gt1
LEFT JOIN gt2
ON gt1.age_group = gt2.age_group
AND round(gt1.pregnancy_start_date/28,0) = round(gt2.pregnancy_start_date/28,0) 
AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign.db")

resultsp <- sqldf("select row_number() over (order by 'round1p.age_group') as matched_id, round1p.exposed_id, round1p.control1_id, round2p.control2_id, round3p.control3_id,
      round1p.age_group,
      round1p.pregnancy_start_date
      from round1p, round2p, round3p
      where round1p.exposed_id = round2p.exposed_id
      and round1p.exposed_id = round3p.exposed_id
      GROUP BY round1p.exposed_id
  --    HAVING MIN(round1p.pregnancy_start_date)
      ORDER BY round1p.pregnancy_start_date" , dbname = "consign.db")

# write to csv
fwrite(resultsp, paste0(matched_folder,"matched_pregnant.csv"))

