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
# visualize the cohorts (after matching?) 

pan_preg_neg<-fread(paste0(cov_neg_pan_preg_folder, "cov_neg_preg.csv"))
pan_preg_neg_dates <- table(cut(as.Date(pan_preg_neg$pregnancy_end_date), "month"))
my_ylim<-(max(pan_preg_neg_dates))+1
levels(pan_preg_neg_dates)<-seq(start_study_date,end_study_date,by="month")

pan_preg_pos<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))
pan_preg_pos_dates <- table(cut(as.Date(pan_preg_pos$pregnancy_end_date), "month"))
levels(pan_preg_pos_dates)<-seq(start_study_date,end_study_date,by="month")




hist_preg<-fread(paste0(hist_preg_folder, "my_PREG.csv"))
hist_end_dates<-cut(as.Date(hist_preg$pregnancy_end_date), "month" )
levels(hist_end_dates)<-seq(start_study_date,end_study_date,by="month")



not_preg_pos<- fread(paste0(not_preg_folder, "covid_positive/PERSONS.csv"))
# not_preg_pos<-

# pan_preg : hist of preg_end_dates

# want to layer data over the whole study period

plot(0, ylim=c(0,my_ylim), xlim=c(start_study_date, end_study_date), type="n",xaxt="n", xlab="", ylab="", main="Pregnancy Cohorts")

# i want the xlab to show month-year from 01-01-2018 to 31-12-2021 
xlab_dat <- format(seq(start_study_date,end_study_date,by="month"), "%Y-%b")

axis(1, at=(as.numeric(seq(start_study_date,end_study_date,by="month"))), xlab_dat, las=2)

abline(v= pan_start_date, col=2, lty=2)

rect((pan_start_date),0, (pan_start_date-91),my_ylim, col = rgb(0.5,0.5,0.5,1/4), border =NA, lwd=0.1)

lines(hist_preg)
