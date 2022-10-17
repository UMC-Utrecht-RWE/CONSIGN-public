# Matching instructions
# These person_ids can be matched using the pregnancy tables, variables are 
#  1)pregnancy_start_date   
#  2)age_group 
#  \CDMInstances_pan_pregnant\covid_negative==cov_neg_pan_preg_folder
#  \CDMInstances_pan_pregnant\covid_positive==cov_pos_pan_preg_folder
#
#These need to be matched by cov_date  (and age? age_at_cov_date? Eimir) (age_at_cov_date needs to be calculated)
#  \CDMInstances_pan_pregnant\covid_positive== cov_pos_pan_preg_folder
#  \CDMInstances_not_pregnant\covid_positive==  cov_pos_not_preg_folder
#
library(RSQLite)
library(sqldf)
library(rstudioapi)
rm(list=ls())

#Set pipeline project directory with path to source code folder 
# to be used by all other folder definition
projectDir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectDir)

#Set source folder for data sources 
# based on projectDir, for both populations
dataPregPosDir<-paste0(projectDir,"/CDMInstances_pan_pregnant/covid_positive/")  
dataNotPregDir<-paste0(projectDir,"/CDMInstances_not_pregnant/covid_positive/")  
dataPregNegDir<-paste0(projectDir,"/CDMInstances_pan_pregnant/covid_negative/")  

# read exposed file
t1 <- read.csv(paste0(dataPregPosDir,"cov_pos_preg.csv"))
# read control file
t2a <- read.csv(paste0(dataNotPregDir,"covid_data_not_preg.csv"))
# read control file
t2b <- read.csv(paste0(dataNotPregDir,"PERSONS.csv"))
t2 <-sqldf("select t2a.person_id, t2b.age_group, t2a.cov_date from t2a, t2b 
          where t2a.person_id = t2b.person_id ")

# look at first three lines to test
sqldf("select * from t1 limit 3")
sqldf("select * from t2 limit 3")

# 111111111111111111111111111111111111111111111111111111111
# execute matching: 1st round 
# 111111111111111111111111111111111111111111111111111111111
round1 <-sqldf(
  "WITH 
gt1 AS (
  SELECT
  person_id,
  age_group,
  covid_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, covid_date ORDER BY person_id) AS a_row
  FROM t1), 
gt2 AS (
  SELECT
  person_id,
  age_group,
  cov_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, cov_date ORDER BY person_id) AS b_row
  FROM t2
)
SELECT gt1.person_id exposed_id,
       gt2.person_id control1_id,
       gt2.age_group,
       gt2.cov_date
FROM gt1
LEFT JOIN gt2
ON gt1.age_group = gt2.age_group
   AND gt1.covid_date between gt2.cov_date-7 and gt2.cov_date+7
   AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign.db")


#22222222222222222222222222222222222222222222222222222222
# execute matching: 2st round 
#22222222222222222222222222222222222222222222222222222222
round2 <-sqldf(
  
  "WITH 
gt1 AS (
  SELECT
  person_id ,
  age_group,
  covid_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, covid_date ORDER BY person_id) AS a_row
  FROM t1), 

gt2 AS (
  SELECT
  person_id,
  age_group,
  cov_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, cov_date ORDER BY person_id) AS b_row
  FROM t2
  WHERE t2.person_id NOT IN (select control1_id from round1)

)
SELECT gt1.person_id exposed_id,
gt2.person_id control2_id,
gt2.age_group,
gt2.cov_date
FROM gt1
LEFT JOIN gt2
ON gt1.age_group = gt2.age_group
AND gt1.covid_date between gt2.cov_date-7 and gt2.cov_date+7

AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign.db", verbose=TRUE)

#333333333333333333333333333333333333333333333333333333333333333333333333
# execute matching: 3st round 
#333333333333333333333333333333333333333333333333333333333333333333333333
round3 <- sqldf(
  
  "WITH 
gt1 AS (
  SELECT
  person_id,
  age_group,
  covid_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, covid_date ORDER BY person_id) AS a_row
  FROM t1), 

gt2 AS (
  SELECT
  person_id,
  age_group,
  cov_date,
  ROW_NUMBER() OVER (PARTITION BY age_group, cov_date ORDER BY person_id) AS b_row
  FROM t2
  WHERE t2.person_id NOT IN (select control1_id from round1)
  AND t2.person_id NOT IN (select control2_id from round2)
)
SELECT gt1.person_id exposed_id,
gt2.person_id control3_id,
gt2.age_group,
gt2.cov_date
FROM gt1
LEFT JOIN gt2
ON gt1.age_group = gt2.age_group
AND gt1.covid_date between gt2.cov_date-7 and gt2.cov_date+7
AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign.db")

results <- sqldf("select round1.exposed_id, round1.control1_id, round2.control2_id, round3.control3_id,
      round1.age_group,
      round1.cov_date
      from round1, round2, round3
      where round1.exposed_id = round2.exposed_id
      and round1.exposed_id = round3.exposed_id
GROUP BY round1.exposed_id
HAVING MIN(round1.cov_date)
ORDER BY round1.cov_date
                 
                 ", dbname = "consign.db")




# write to csv
write.csv(results,'matching_results_not_preg.csv')

