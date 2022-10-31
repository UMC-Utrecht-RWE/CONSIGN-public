#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
# 19/10/2022

# cases are covid positve pregnancies

# cases have been matched 3:1 to cov+ but NOT pregnant and pregnant but covid negative

# covid negative pregnant controls need cov_date and cov_trimester  of their case added to their pregnancy file

# covid positive NON PREGNANT controls need start_date_pregnancy and cov_trimester from their case (where to add it?)
# --> take their covid dates, case preg dates and make a new dataset

keep_vars<-c("exposed_id","control1_id","control2_id","control3_id","age_group")

covid_positive_matched<-as.data.frame(fread(paste0(matched_folder, "matched_covid_postive.csv")))
covid_positive_matched<-covid_positive_matched[keep_vars]
covid_positive_matched<-covid_positive_matched %>% mutate_all(na_if,"")
cov_data_non_preg_controls<-fread(paste0(cov_pos_not_preg_folder, "covid_data_not_preg.csv"))

pregnant_matched<-as.data.frame(fread(paste0(matched_folder, "matched_pregnant.csv")))
pregnant_matched<-pregnant_matched[keep_vars]
pregnant_matched<-pregnant_matched %>% mutate_all(na_if,"")
preg_data_cov_neg<-fread(paste0(cov_neg_pan_preg_folder, "cov_neg_preg.csv"))
#3 columns on the end with empty covid data (because they don't have covid) need to remove them
preg_data_cov_neg<-preg_data_cov_neg[,-c(43,44,45)]
# need to have only one pregnancy from each control mother
preg_control_grouped<-preg_data_cov_neg%>%group_by(person_id)
preg_data_cov_neg<-as.data.frame(ungroup(preg_control_grouped%>%slice_min(n = 1, order_by = pregnancy_start_date)))


case_data_all<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))
case_data_grouped<-case_data_all%>%group_by(person_id)
case_data<-case_data_grouped%>%slice_min(n = 1, order_by = pregnancy_start_date)


# make sure order is good
covid_positive_matched<-covid_positive_matched[order(covid_positive_matched$exposed_id),]
pregnant_matched<-pregnant_matched[order(pregnant_matched$exposed_id),]
case_data<-case_data[order(case_data$person_id),]

nrow(covid_positive_matched)
nrow(pregnant_matched)
length(case_data$person_id)
length(unique(case_data$person_id))

all(covid_positive_matched$exposed_id==case_data$person_id)
all(pregnant_matched$exposed_id==case_data$person_id)

#########################################################

# transfer dates from cases to controls

covid_positive_matched$pregnancy_start_date<-case_data$pregnancy_start_date
covid_positive_matched$cov_trimester<-case_data$cov_trimester

pregnant_matched$cov_date<-case_data$covid_date
pregnant_matched$cov_trimester<-case_data$cov_trimester

######################################################
# wide to long

long_covid_match<-melt(covid_positive_matched, id.vars=c("exposed_id", "age_group", "pregnancy_start_date","cov_trimester"))
long_covid_match$control_id<-long_covid_match$value
long_covid_match<-long_covid_match[order(long_covid_match$control_id),]
long_covid_match<-long_covid_match[complete.cases(long_covid_match),]

long_pregnant_match<-melt(pregnant_matched, id.vars=c("exposed_id", "age_group", "cov_date","cov_trimester"))
long_pregnant_match$control_id<-long_pregnant_match$value
long_pregnant_match<-long_pregnant_match[order(long_pregnant_match$control_id),]
long_pregnant_match<-long_pregnant_match[complete.cases(long_pregnant_match),]
######################################################

covid_match_data<-cov_data_non_preg_controls[cov_data_non_preg_controls$person_id%in%long_covid_match$control_id]
covid_match_data<-covid_match_data[order(covid_match_data$person_id),]
all(covid_match_data$person_id==long_covid_match$control_id)

transfer_vars<-c("age_group", "exposed_id", "pregnancy_start_date", "cov_trimester")
covid_match_with_preg_dates<-cbind(covid_match_data, long_covid_match[,transfer_vars])

# make sure we have covid dates with cov_date and covid_date names because inconsistency messes things up downstream
covid_match_with_preg_dates$covid_date<-covid_match_with_preg_dates$cov_date

fwrite(covid_match_with_preg_dates, paste0(matched_folder,"matches_cov_pos_non_preg.csv"))

pregnant_match_data<-preg_data_cov_neg[(preg_data_cov_neg$person_id%in%long_pregnant_match$control_id),]
pregnant_match_data<-pregnant_match_data[order(pregnant_match_data$person_id),]
all(pregnant_match_data$person_id==long_pregnant_match$control_id)

transfer_vars<-c("age_group", "exposed_id", "cov_date", "cov_trimester")
pregnant_match_with_covid_dates<-cbind(pregnant_match_data, long_pregnant_match[,transfer_vars])

# make sure we have covid dates with cov_date and covid_date names because inconsistency messes things up downstream
pregnant_match_with_covid_dates$covid_date<-pregnant_match_with_covid_dates$cov_date

fwrite(pregnant_match_with_covid_dates, paste0(matched_folder, "matches_pregnant_cov_neg.csv"))
