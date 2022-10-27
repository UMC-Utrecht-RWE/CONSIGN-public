# tables 2e-f 3e-f

# matched cohort drug utilization

# 3 d e and f are trim 1, 2, 3 covid+ non-preg matched cohort drug utilization


rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

# CHECK/ FILL IN YOUR PARAMETERS FOR PREGNANCY ALGORITHM FILE 

source("params.R")

source("99_path.R")

suppressMessages(source(paste0(pre_dir, "/packages.R")))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

source(paste0(pre_dir, "/creat_table_3abc.R"))

source(paste0(pre_dir, "/cov_window_exposure_function.R"))

source(paste0(pre_dir, "/trimester_drug_exposure_NP_matches.R"))

source(paste0(pre_dir, "/create_table_3def.R"))
