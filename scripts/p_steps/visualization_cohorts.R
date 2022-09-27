# cohort description/visualization

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
