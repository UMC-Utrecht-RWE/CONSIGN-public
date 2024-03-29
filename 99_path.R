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



path_dir<-paste0(projectFolder,"/scripts/")
if(dir.exists(path_dir)==F){dir.create(path_dir)}


meta_dir<-paste0(projectFolder,"/meta_analysis_tables/")
if(dir.exists(meta_dir)==F){dir.create(meta_dir)}

matched_folder<-paste0(projectFolder,"/matched/")
invisible(if(dir.exists(matched_folder)==F)
{dir.create(matched_folder)})

preg_match_folder<-paste0(projectFolder,"/matched/CDM_preg_negative/")
invisible(if(dir.exists(preg_match_folder)==F)
{dir.create(preg_match_folder)})

cases_match_folder<-paste0(projectFolder,"/matched/CDM_preg_positive/")
invisible(if(dir.exists(cases_match_folder)==F)
{dir.create(cases_match_folder)})

cov_match_folder<-paste0(projectFolder,"/matched/CDM_covid_positive/")
invisible(if(dir.exists(cov_match_folder)==F)
{dir.create(cov_match_folder)})

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


# 
case_neonate_folder<-paste0(projectFolder,"/CDMInstances_case_neonate/")
invisible(if(dir.exists(case_neonate_folder)==F)
{dir.create(case_neonate_folder)})

control_neonate_folder<-paste0(projectFolder,"/CDMInstances_control_neonate/")
invisible(if(dir.exists(control_neonate_folder)==F)
{dir.create(control_neonate_folder)})

historical_neonate_folder<-paste0(projectFolder,"/CDMInstances_historical_neonate/")
invisible(if(dir.exists(historical_neonate_folder)==F)
{dir.create(historical_neonate_folder)})


# Checks if folders exist. If they do not, creates them 
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/")), dir.create(paste0(projectFolder, "/g_intermediate")), FALSE))
g_intermediate <- paste0(projectFolder, "/g_intermediate/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/covariates/")), dir.create(paste0(projectFolder, "/g_intermediate/covariates/")), FALSE))
g_intermediate_covariates <- paste0(projectFolder, "/g_intermediate/covariates/")


invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
output_dir     <- paste0(projectFolder, "/g_output/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/neonates/")), dir.create(paste0(projectFolder, "/g_output/neonates/")), FALSE))
output_neonates     <- paste0(projectFolder, "/g_output/neonates/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/neonates/cases/")), dir.create(paste0(projectFolder, "/g_output/neonates/cases/")), FALSE))
output_neonates_case     <- paste0(projectFolder, "/g_output/neonates/cases/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/neonates/controls/")), dir.create(paste0(projectFolder, "/g_output/neonates/controls/")), FALSE))
output_neonates_control     <- paste0(projectFolder, "/g_output/neonates/controls/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/neonates/historical/")), dir.create(paste0(projectFolder, "/g_output/neonates/historical")), FALSE))
output_neonates_hist    <- paste0(projectFolder, "/g_output/neonates/historical/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/final/")), dir.create(paste0(projectFolder, "/g_output/final/")), FALSE))
final_output_dir     <- paste0(projectFolder, "/g_output/final/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/final/supplement/")), dir.create(paste0(projectFolder, "/g_output/final/supplement/")), FALSE))
final_output_suppl    <- paste0(projectFolder, "/g_output/final/supplement/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/covid_comorbidities_events/")), dir.create(paste0(projectFolder, "/g_intermediate/covid_comorbidities_events")), FALSE))
cov_comorbid_events   <- paste0(projectFolder, "/g_intermediate/covid_comorbidities_events/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/maternal_covariates_events/")), dir.create(paste0(projectFolder, "/g_intermediate/maternal_covariates_events/")), FALSE))
maternal_covariates_events   <- paste0(projectFolder, "/g_intermediate/maternal_covariates_events/")

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

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/cov_window_atc_3/")), dir.create(paste0(projectFolder, "/g_output/cov_window_atc_3")), FALSE))
output_cov_window_atc_3    <- paste0(projectFolder, "/g_output/cov_window_atc_3/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/cov_window_atc_4/")), dir.create(paste0(projectFolder, "/g_output/cov_window_atc_4")), FALSE))
output_cov_window_atc_4    <- paste0(projectFolder, "/g_output/cov_window_atc_4/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/cov_window_atc_5/")), dir.create(paste0(projectFolder, "/g_output/cov_window_atc_5")), FALSE))
output_cov_window_atc_5    <- paste0(projectFolder, "/g_output/cov_window_atc_5/")


invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/trimester/")), dir.create(paste0(projectFolder, "/g_output/trimester")), FALSE))
output_trimester    <- paste0(projectFolder, "/g_output/trimester/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/")), dir.create(paste0(projectFolder, "/g_output/covariates")), FALSE))
output_cov   <- paste0(projectFolder, "/g_output/covariates/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/cov_pos_pan_preg/")), dir.create(paste0(projectFolder, "/g_output/covariates/cov_pos_pan_preg/")), FALSE))
output_cov_cases   <- paste0(projectFolder, "/g_output/covariates/cov_pos_pan_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/cov_neg_pan_preg/")), dir.create(paste0(projectFolder, "/g_output/covariates/cov_neg_pan_preg/")), FALSE))
output_cov_pregnant_control <- paste0(projectFolder, "/g_output/covariates/cov_neg_pan_preg/")


invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/covariates/cov_pos_non_preg/")), dir.create(paste0(projectFolder, "/g_output/covariates/cov_pos_non_preg/")), FALSE))
output_cov_nonpregnant_control   <- paste0(projectFolder, "/g_output/covariates/cov_pos_non_preg/")

output_CONTROL_NP_window_atc_2<-paste0(projectFolder,"/g_output/NP_CONTROL_window_atc_2/")
invisible(if(dir.exists(output_CONTROL_NP_window_atc_2)==F)
{dir.create(output_CONTROL_NP_window_atc_2)})

output_CONTROL_PREG_window_atc_2<-paste0(projectFolder,"/g_output/PREG_CONTROL_window_atc_2/")
invisible(if(dir.exists(output_CONTROL_PREG_window_atc_2)==F)
{dir.create(output_CONTROL_PREG_window_atc_2)})

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/covariates/cov_pos_pan_preg/")), dir.create(paste0(projectFolder, "/g_intermediate/covariates/cov_pos_pan_preg/")), FALSE))
output_cov_pos_pan_preg   <- paste0(projectFolder, "/g_intermediate/covariates/cov_pos_pan_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/covariates/cov_neg_pan_preg/")), dir.create(paste0(projectFolder, "/g_intermediate/covariates/cov_neg_pan_preg/")), FALSE))
output_cov_neg_pan_preg   <- paste0(projectFolder, "/g_intermediate/covariates/cov_neg_pan_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/covariates/cov_pos_non_preg/")), dir.create(paste0(projectFolder, "/g_intermediate/covariates/cov_pos_non_preg/")), FALSE))
output_cov_pos_non_preg   <- paste0(projectFolder, "/g_intermediate/covariates/cov_pos_non_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/covariates/hist_preg/")), dir.create(paste0(projectFolder, "/g_intermediate/covariates/hist_preg/")), FALSE))
output_hist_preg   <- paste0(projectFolder, "/g_intermediate/covariates/hist_preg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/maternal_covariates/")), dir.create(paste0(projectFolder, "/g_intermediate/maternal_covariates")), FALSE))
output_mat_cov   <- paste0(projectFolder, "/g_intermediate/maternal_covariates/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/maternal_covariates/pan_pos/")), dir.create(paste0(projectFolder, "/g_intermediate/maternal_covariates/pan_pos/")), FALSE))
output_mat_cov_pan_pos   <- paste0(projectFolder, "/g_intermediate/maternal_covariates/pan_pos/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/maternal_covariates/pan_neg/")), dir.create(paste0(projectFolder, "/g_intermediate/maternal_covariates/pan_neg/")), FALSE))
output_mat_cov_pan_neg   <- paste0(projectFolder, "/g_intermediate/maternal_covariates/pan_neg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/maternal_covariates/hist/")), dir.create(paste0(projectFolder, "/g_intermediate/maternal_covariates/hist/")), FALSE))
output_mat_cov_hist   <- paste0(projectFolder, "/g_intermediate/maternal_covariates/hist/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate/maternal_covariates/covid_positive_match/")), dir.create(paste0(projectFolder, "/g_intermediate/maternal_covariates/covid_positive_match/")), FALSE))
output_mat_cov_covid_pos_match   <- paste0(projectFolder, "/g_intermediate/maternal_covariates/covid_positive_match/")



invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates")), FALSE))
g_output_mat_cov   <- paste0(projectFolder, "/g_output/maternal_covariates/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/pan_pos/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates/pan_pos")), FALSE))
g_output_mat_cov_pan_pos   <- paste0(projectFolder, "/g_output/maternal_covariates/pan_pos/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/pan_neg/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates/pan_neg")), FALSE))
g_output_mat_cov_pan_neg   <- paste0(projectFolder, "/g_output/maternal_covariates/pan_neg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/hist/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates/hist")), FALSE))
g_output_mat_cov_hist   <- paste0(projectFolder, "/g_output/maternal_covariates/hist/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_covariates/covid_positive_matches/")), dir.create(paste0(projectFolder, "/g_output/maternal_covariates/covid_positive_matches")), FALSE))
g_output_mat_covariate_covid_pos_match   <- paste0(projectFolder, "/g_output/maternal_covariates/covid_positive_matches/")



invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_outcomes/")), dir.create(paste0(projectFolder, "/g_output/maternal_outcomes")), FALSE))
g_output_mat_out   <- paste0(projectFolder, "/g_output/maternal_outcomes/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_outcomes/pan_pos/")), dir.create(paste0(projectFolder, "/g_output/maternal_outcomes/pan_pos")), FALSE))
g_output_mat_out_pan_pos   <- paste0(projectFolder, "/g_output/maternal_outcomes/pan_pos/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_outcomes/pan_neg/")), dir.create(paste0(projectFolder, "/g_output/maternal_outcomes/pan_neg")), FALSE))
g_output_mat_out_pan_neg   <- paste0(projectFolder, "/g_output/maternal_outcomes/pan_neg/")

invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output/maternal_outcomes/hist/")), dir.create(paste0(projectFolder, "/g_output/maternal_outcomes/hist")), FALSE))
g_output_mat_out_hist   <- paste0(projectFolder, "/g_output/maternal_outcomes/hist/")



# Sets path to p_steps (to read codelists)
pre_dir <- paste0(path_dir,"p_steps")
invisible(ifelse(!dir.exists(paste0(path_dir, "p_steps")), dir.create(paste0(path_dir, "p_steps")), FALSE))
# folders + paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "populations")), dir.create(paste0(g_intermediate, "populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "tmp", sep="")), dir.create(paste0(g_intermediate, "tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")



