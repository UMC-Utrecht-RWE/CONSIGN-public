# cohort description/visualization

# 
# non_pregancy checklist
# -NOT pregnant btween 1/3/20 and 31/12/20 (full year follow up to end of study)
# 
# - problem is that in birth registry we don't know the start of pregnancy until birth
# --> SO if a birth occurs 1/1/21 (exlcuded from my_PREG) so the WILL end up in non-preg
#  --> from RAW my_PREG all pregnancies not pregnant EVER 
#   --> NOT pregnant in 2020 from march-december (preg_end_date< march 1 2020) --> non_preg cohort
#    --> person_id %in% cov_data (only want cov pos)
# 
# #pregnancy checklist
# 
# visualize the cohorts 


# pan_preg : hist of preg_end_dates

# want to layer data over the whole study period

plot(0, ylim=c(0,0.3), xlim=c(study_start_date, end_study_date), type="n", xaxt="n", xlab="", ylab="")

pan_preg_neg<-fread(paste0(cov_neg_pan_preg_folder, "cov_neg_preg.csv"))
pan_preg_neg <- table(cut(as.Date(pan_preg_neg$pregnancy_end_date), "month"))
hist((as.Date(pan_preg_neg$pregnancy_end_date)), breaks=100, freq = F )

pan_preg_pos<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))
as.Date(pan_preg_pos$pregnancy_end_date)
hist((as.Date(pan_preg_pos$pregnancy_end_date)), breaks=100, freq = F )

pan_preg_neg<-fread(paste0(cov_neg_pan_preg_folder, "cov_neg_preg.csv"))
as.Date(pan_preg_neg$pregnancy_end_date)
hist((as.Date(pan_preg_neg$pregnancy_end_date)), breaks=100, freq = F )

not_preg_pos<- fread(paste0(not_preg_folder, "covid_positive/PERSONS.csv"))
