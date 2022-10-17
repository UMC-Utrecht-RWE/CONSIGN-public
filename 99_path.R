#Author:Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 01/03/2022

#This script sets and saves paths to folders needed for all subsequent scripts
# setwd('..') #in Data Characterisation

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
# 
# setwd('..') 
# dir_base<-getwd()
# set the name of the study
StudyName <- "CONSIGN"

if(set_my_CDM==F){
  path_CDM<-paste0(projectFolder,"/CDMInstances/")
  invisible(if(dir.exists(path_CDM)==F)
  {dir.create(path_CDM)})}

if(set_my_CDM==T){path_CDM<-paste0(my_path_CDM)
  invisible(if(dir.exists(path_CDM)==F)
  {dir.create(path_CDM)})}


path_dir<-paste0(projectFolder,"/scripts/")
if(dir.exists(path_dir)==F){dir.create(path_dir)}

matched_folder<-paste0(projectFolder,"/matched/")
invisible(if(dir.exists(matched_folder)==F)
{dir.create(matched_folder)})


pan_preg_folder<-paste0(projectFolder,"/CDMInstances_pan_pregnant/")
invisible(if(dir.exists(pan_preg_folder)==F)
{dir.create(pan_preg_folder)})

cov_pos_pan_preg_folder<-paste0(projectFolder,"/CDMInstances_pan_pregnant/covid_positive/")
invisible(if(dir.exists(cov_pos_pan_preg_folder)==F)
{dir.create(cov_pos_pan_preg_folder)})

cov_neg_pan_preg_folder<-paste0(projectFolder,"/CDMInstances_pan_pregnant/covid_negative/")
invisible(if(dir.exists(cov_neg_pan_preg_folder)==F)
{dir.create(cov_neg_pan_preg_folder)})

hist_preg_folder<-paste0(projectFolder,"/CDMInstances_hist_pregnant/")
invisible(if(dir.exists(hist_preg_folder)==F)
{dir.create(hist_preg_folder)})

not_preg_folder<-paste0(projectFolder,"/CDMInstances_not_pregnant/")
invisible(if(dir.exists(not_preg_folder)==F)
{dir.create(not_preg_folder)})

cov_pos_not_preg_folder<-paste0(projectFolder,"/CDMInstances_not_pregnant/covid_positive/")
invisible(if(dir.exists(cov_pos_not_preg_folder)==F)
{dir.create(cov_pos_not_preg_folder)})

cov_neg_not_preg_folder<-paste0(projectFolder,"/CDMInstances_not_pregnant/covid_negative/")
invisible(if(dir.exists(cov_neg_not_preg_folder)==F)
{dir.create(cov_neg_not_preg_folder)})


preselect_folder<-paste0(projectFolder,"/CDMInstances_preselect/")
invisible(if(dir.exists(preselect_folder)==F)
{dir.create(preselect_folder)})

# Checks if folders exist. If they do not, creates them 
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/")), dir.create(paste0(projectFolder, "/g_intermediate")), FALSE))
g_intermediate <- paste0(projectFolder, "/g_intermediate/")


invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
output_dir     <- paste0(projectFolder, "/g_output/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/final/")), dir.create(paste0(projectFolder, "/g_output/final/")), FALSE))
final_output_dir     <- paste0(projectFolder, "/g_output/final/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/atc_2_counts/")), dir.create(paste0(projectFolder, "/g_intermediate/atc_2_counts")), FALSE))
raw_atc_2_counts    <- paste0(projectFolder, "/g_intermediate/atc_2_counts/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/atc_3_counts/")), dir.create(paste0(projectFolder, "/g_intermediate/atc_3_counts")), FALSE))
raw_atc_3_counts    <- paste0(projectFolder, "/g_intermediate/atc_3_counts/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/atc_4_counts/")), dir.create(paste0(projectFolder, "/g_intermediate/atc_4_counts")), FALSE))
raw_atc_4_counts    <- paste0(projectFolder, "/g_intermediate/atc_4_counts/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/atc_5_counts/")), dir.create(paste0(projectFolder, "/g_intermediate/atc_5_counts")), FALSE))
raw_atc_5_counts    <- paste0(projectFolder, "/g_intermediate/atc_5_counts/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/cov_window_atc_2/")), dir.create(paste0(projectFolder, "/g_output/cov_window_atc_2")), FALSE))
output_cov_window_atc_2    <- paste0(projectFolder, "/g_output/cov_window_atc_2/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/trimester/")), dir.create(paste0(projectFolder, "/g_output/trimester")), FALSE))
output_trimester    <- paste0(projectFolder, "/g_output/trimester/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/")), dir.create(paste0(projectFolder, "/g_output/covariates")), FALSE))
output_cov   <- paste0(projectFolder, "/g_output/covariates/")


invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/cov_pos_pan_preg/")), dir.create(paste0(projectFolder, "/g_output/covariates/cov_pos_pan_preg/")), FALSE))
output_cov_pos_pan_preg   <- paste0(projectFolder, "/g_output/covariates/cov_pos_pan_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/cov_neg_pan_preg/")), dir.create(paste0(projectFolder, "/g_output/covariates/cov_neg_pan_preg/")), FALSE))
output_cov_neg_pan_preg   <- paste0(projectFolder, "/g_output/covariates/cov_neg_pan_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/cov_pos_non_preg/")), dir.create(paste0(projectFolder, "/g_output/covariates/cov_pos_non_preg/")), FALSE))
output_cov_pos_non_preg   <- paste0(projectFolder, "/g_output/covariates/cov_pos_non_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/hist_preg/")), dir.create(paste0(projectFolder, "/g_output/covariates/hist_preg/")), FALSE))
output_hist_preg   <- paste0(projectFolder, "/g_output/covariates/hist_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates")), FALSE))
output_mat_cov   <- paste0(projectFolder, "/g_output/maternal_covariates/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates")), FALSE))
output_mat_cov   <- paste0(projectFolder, "/g_output/maternal_covariates/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/pan_pos/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates/pan_pos")), FALSE))
output_mat_cov_pan_pos   <- paste0(projectFolder, "/g_output/maternal_covariates/pan_pos/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/pan_neg/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates/pan_neg")), FALSE))
output_mat_cov_pan_neg   <- paste0(projectFolder, "/g_output/maternal_covariates/pan_neg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/hist/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates/hist")), FALSE))
output_mat_cov_hist   <- paste0(projectFolder, "/g_output/maternal_covariates/hist/")


# Sets path to p_steps (to read codelists)
pre_dir <- paste0(path_dir,"p_steps")
invisible(ifelse(!dir.exists(paste0(path_dir, "p_steps")), dir.create(paste0(path_dir, "p_steps")), FALSE))
# folders + paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "populations")), dir.create(paste0(g_intermediate, "populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "tmp", sep="")), dir.create(paste0(g_intermediate, "tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")



