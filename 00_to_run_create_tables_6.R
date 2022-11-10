#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
# 8/11/2022


# make sure you have covid_data.csv in preselect_folder
# make sure you have ALL_full_codelist.csv in projectFolder

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

# CHECK/ FILL IN YOUR PARAMETERS 

source("params.R")

source("99_path.R")

# there are NEW PACKAGES for the modelling- make sure up to date (20:45 8/11)
suppressMessages(source(paste0(pre_dir, "/packages.R")))

# "sandwich" can be tricky to install- you may need to restart R
# it also can take some time to install "sandwich" for me it was about 3 minutes


# you may see this message in RED- it's OK- it just means that this outcome has no observations
# Error in prop.test(trim1[[1]], trim1[[2]]) : 
#   elements of 'n' must be positive
# In addition: Warning messages:
#   1: In prop.test(trim1[[1]], trim1[[2]]) :
#   Chi-squared approximation may be incorrect
# 2: In prop.test(trim1[[1]], trim1[[2]]) :
#   Chi-squared approximation may be incorrect


# if the Chi Squared warning ocurrs for many outcomes, you will see this in RED:
# There were 50 or more warnings (use warnings() to see the first 50)
# it's OK, just means the numerator was 0 (the outcome didn't happen in that cohort/timeframe)

source(paste0(pre_dir, "/create_table_6_maternal_data.R"))

# when there are 0 observations- prop.test prints a warning about Chi squared test- it's OK

source(paste0(pre_dir, "/create_table_6_gest_diab.R"))

source(paste0(pre_dir, "/create_table_6_preeclamp.R"))

source(paste0(pre_dir, "/create_table_6_preterm.R"))

# clear environment between maternal and neonatal to prevent incorrect object assignment

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

# CHECK/ FILL IN YOUR PARAMETERS 

source("params.R")

source("99_path.R")

source(paste0(pre_dir, "/create_table_6_neonate_data.R"))

source(paste0(pre_dir, "/create_table_6_LBW.R"))






