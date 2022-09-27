# Matching instructions
# These can be matched using the pregnancy tables, variables are 
#  1)pregnancy_start_date (it's in numeric format using index date default from "zoo" package) and then  
#  2)age_group (categorical, 1,2 or 3) 
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

#Set pipeline project directory with path to source code folder (to be used by all other folder definition)
projectDir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectDir)

#Set source folder for data sources based on projectDir
dataDir<-paste0(projectDir,"/CDMInstances_pan_pregnant/")  

# read exposed file
t1 <- read.csv(paste0(dataDir,"p1.csv"))
# read control file
t2 <- read.csv(paste0(dataDir,"p2.csv"))


# create an empty database called consign
sqldf("attach consign as new")
#sqldf("drop table main.t1")

# create table inside database to store p1 and p2 data
read.csv.sql("p1.csv", sql = "create table main.t1 as select * from file", 
             dbname = "consign")

read.csv.sql("p2.csv", sql = "create table main.t2 as select * from file", 
             dbname = "consign")

# look at first three lines to test
sqldf("select * from main.t1 limit 3", dbname = "consign")
sqldf("select * from main.t2 limit 3", dbname = "consign")




# 111111111111111111111111111111111111111111111111111111111
# execute matching: 1st round 
# 11111111111111111111111111111111111111111111111111111111

sqldf(

"WITH 
gt1 AS (
  SELECT
  person_id,
  year_of_birth,
  sex_at_instance_creation,
  ROW_NUMBER() OVER (PARTITION BY year_of_birth, sex_at_instance_creation ORDER BY person_id) AS a_row
  FROM main.t1), 

gt2 AS (
  SELECT
  person_id,
  year_of_birth,
  sex_at_instance_creation,
  ROW_NUMBER() OVER (PARTITION BY year_of_birth, sex_at_instance_creation ORDER BY person_id) AS b_row
  FROM main.t2
)
SELECT gt1.person_id exposed_id,
gt2.person_id control1_id,
gt2.year_of_birth,
gt2.sex_at_instance_creation
FROM gt1
LEFT JOIN gt2
ON gt1.year_of_birth = gt2.year_of_birth
AND gt1.sex_at_instance_creation = gt2.sex_at_instance_creation
AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign")

#22222222222222222222222222222222222222222222222222222222
# execute matching: 2st round 
#22222222222222222222222222222222222222222222222222222222
sqldf(
  
  "WITH 
gt1 AS (
  SELECT
  person_id ,
  year_of_birth,
  sex_at_instance_creation,
  ROW_NUMBER() OVER (PARTITION BY year_of_birth, sex_at_instance_creation ORDER BY person_id) AS a_row
  FROM main.t1), 

gt2 AS (
  SELECT
  person_id,
  year_of_birth,
  sex_at_instance_creation,
  ROW_NUMBER() OVER (PARTITION BY year_of_birth, sex_at_instance_creation ORDER BY person_id) AS b_row
  FROM main.t2
)
SELECT gt1.person_id exposed_id,
gt2.person_id control2_id,
gt2.year_of_birth,
gt2.sex_at_instance_creation
FROM gt1
LEFT JOIN gt2
ON gt1.year_of_birth = gt2.year_of_birth
AND gt1.sex_at_instance_creation = gt2.sex_at_instance_creation
AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign")

#333333333333333333333333333333333333333333333333333333333333333333333333
# execute matching: 3st round 
#333333333333333333333333333333333333333333333333333333333333333333333333
sqldf(
  
  "WITH 
gt1 AS (
  SELECT
  person_id,
  year_of_birth,
  sex_at_instance_creation,
  ROW_NUMBER() OVER (PARTITION BY year_of_birth, sex_at_instance_creation ORDER BY person_id) AS a_row
  FROM main.t1), 

gt2 AS (
  SELECT
  person_id,
  year_of_birth,
  sex_at_instance_creation,
  ROW_NUMBER() OVER (PARTITION BY year_of_birth, sex_at_instance_creation ORDER BY person_id) AS b_row
  FROM main.t2
)
SELECT gt1.person_id exposed_id,
gt2.person_id control3_id,
gt2.year_of_birth,
gt2.sex_at_instance_creation
FROM gt1
LEFT JOIN gt2
ON gt1.year_of_birth = gt2.year_of_birth
AND gt1.sex_at_instance_creation = gt2.sex_at_instance_creation
AND a_row = b_row
ORDER BY gt2.person_id"

, dbname = "consign")

