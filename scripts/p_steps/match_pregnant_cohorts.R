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
#t1p <- read.csv(paste0(dataPregPosDir,"cov_pos_preg.csv"))

t1p_all <- read.csv(paste0(dataPregPosDir,"cov_pos_preg.csv"))
t1p_grouped<-t1p_all%>%group_by(person_id)
t1p<-t1p_grouped%>%slice_min(n = 1, order_by = pregnancy_start_date)
t1p<-ungroup(t1p)

fwrite(t1p, paste0( matched_folder, "matches_cases.csv"))
# # read control file
# t2p <- read.csv(paste0(dataNotPregDir,"PERSONS.csv"))
# read control file

# adding line to select cov_neg_pan_preg to take FIRST pregnancy so we don't select same mother twice as control

t2p_all <- read.csv(paste0(dataPregNegDir,"cov_neg_preg.csv"))
t2p_grouped<-t2p_all%>%group_by(person_id)


t2p<-t2p_grouped%>%slice_min(n = 1, order_by = pregnancy_start_date)


# look at first three lines to test
sqldf("select * from t1p limit 3")
# sqldf("select * from t2p limit 3")
sqldf("select * from t2p limit 3")

# 111111111111111111111111111111111111111111111111111111111
# execute matching: 1st round 
# 11111111111111111111111111111111111111111111111111111111

round1p <-sqldf(

"WITH 
gt1p AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0) ORDER BY RANDOM() ) AS a_row
  FROM t1p), 
gt2p AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0)  ORDER BY RANDOM()) AS b_row
  FROM t2p
)
SELECT 
       gt1p.person_id exposed_id,
       gt2p.person_id control1_id,
       gt2p.age_group,
       gt2p.pregnancy_start_date
FROM gt1p
LEFT JOIN gt2p
ON gt1p.age_group = gt2p.age_group
   AND round(gt1p.pregnancy_start_date/28,0) = round(gt2p.pregnancy_start_date/28,0) 
   AND a_row = b_row
ORDER BY gt2p.person_id", dbname = "consign.db")


#22222222222222222222222222222222222222222222222222222222
# execute matching: 2st round 
#22222222222222222222222222222222222222222222222222222222
round2p <-sqldf(
  
  "WITH 
gt1p AS (
  SELECT
  person_id ,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0)  ORDER BY RANDOM()) AS a_row
  FROM t1p), 

gt2p AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0)  ORDER BY RANDOM()) AS b_row
  FROM t2p
  WHERE t2p.person_id NOT IN (select control1_id from round1p)

)
SELECT 
gt1p.person_id exposed_id,
gt2p.person_id control2_id,
gt2p.age_group,
gt2p.pregnancy_start_date
FROM gt1p
LEFT JOIN gt2p
ON gt1p.age_group = gt2p.age_group
AND round(gt1p.pregnancy_start_date/28,0) = round(gt2p.pregnancy_start_date/28,0) 

AND a_row = b_row
ORDER BY gt2p.person_id"

, dbname = "consign.db", verbose=TRUE)



#333333333333333333333333333333333333333333333333333333333333333333333333
# execute matching: 3st round 
#333333333333333333333333333333333333333333333333333333333333333333333333
round3p <- sqldf(
"WITH 
gt1p AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0) ORDER BY RANDOM()) AS a_row
  FROM t1p), 

gt2p AS (
  SELECT
  person_id,
  age_group,
  pregnancy_start_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, round(pregnancy_start_date/28,0) ORDER BY RANDOM()) AS b_row
  FROM t2p
  WHERE t2p.person_id NOT IN (select control1_id from round1p)
  AND t2p.person_id NOT IN (select control2_id from round2p)
)
SELECT 
gt1p.person_id exposed_id,
gt2p.person_id control3_id,
gt2p.age_group,
gt2p.pregnancy_start_date
FROM gt1p
LEFT JOIN gt2p
ON gt1p.age_group = gt2p.age_group
AND round(gt1p.pregnancy_start_date/28,0) = round(gt2p.pregnancy_start_date/28,0) 
AND a_row = b_row
ORDER BY gt2p.person_id"

, dbname = "consign.db")

resultsp <- sqldf("select row_number() over (order by 'round1p.age_group') as matched_id, round1p.exposed_id, round1p.control1_id, round2p.control2_id, round3p.control3_id,
      round1p.age_group,
      round1p.pregnancy_start_date,
      round(round1p.pregnancy_start_date/28,0) as start_month
      from round1p, round2p, round3p
      where round1p.exposed_id = round2p.exposed_id
      and round1p.exposed_id = round3p.exposed_id
      GROUP BY round1p.exposed_id
  --    HAVING MIN(round1p.pregnancy_start_date)
      ORDER BY round1p.pregnancy_start_date" , dbname = "consign.db")

# write to csv
fwrite(resultsp, paste0(matched_folder,"matched_pregnant.csv"))

