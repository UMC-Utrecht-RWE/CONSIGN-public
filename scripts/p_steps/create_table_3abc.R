#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#24/10/2022


# left side of the table is CASES same as in 1abc, but divided trimester-NOT severity

# RIGHT SIDE IS SAME BASELINE CHARACTERISTICS OF PREGNANT Controls BY TRIMESTER


# Number of pregnancies
# Number of unique women
# Gestational age at COVID-19 diagnosis (weeks, median)
# Age, Median
# 12-24
# 25-39
# 40-55
# Calendar month of infection
# Mar-20
# Apr-20

CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

case_data<-fread(paste0(cov_pos_pan_preg_folder,"cov_pos_preg.csv"))
calendar_months<-seq(pan_start_date,end_study_date,by="month")

# 2 issues with control data: 
control_data<-fread(paste0(matched_folder,"matches_cov_pos_non_preg.csv"))
control_data$gest_age_cov<-control_data$cov_date-control_data$pregnancy_start_date
control_persons<-fread(paste0(matched_folder,"CDM_covid_positive/PERSONS.csv"))

all(control_persons$person_id==control_data$person_id)

control_data$age_at_start_of_pregnancy<-control_persons$age_at_start_pandemic

pan_months<-months(calendar_months)
pan_years<-year(calendar_months)

levels_calendar<-paste0(pan_months, "-", pan_years)

trimgroup<-c("a","b","c")

for(i in 1:3){
case_data_trim<-case_data[as.numeric(case_data$cov_trimester)==i,]
control_data_trim<-control_data[as.numeric(control_data$cov_trimester)==i,]

number_preg<-as.numeric(nrow(case_data_trim))
number_moms<-as.numeric(length(unique(case_data_trim$person_id)))

gest_age_at_cov_median<- round( median((case_data_trim$gest_age_cov)/7),1)
gest_age_at_cov_IQR<- round(IQR((case_data_trim$gest_age_cov)/7),1)

age_at_start_preg_median<-round(median(case_data_trim$age_at_start_of_pregnancy),1)
age_IQR<-round(IQR(case_data_trim$age_at_start_of_pregnancy),1)

# between includes min and max in the range >= and <=
age_12_24<-length(case_data_trim$age_at_start_of_pregnancy[between(case_data_trim$age_at_start_of_pregnancy, 12,24)])
age_25_39<-length(case_data_trim$age_at_start_of_pregnancy[between(case_data_trim$age_at_start_of_pregnancy, 25,39)])
age_40_55<-length(case_data_trim$age_at_start_of_pregnancy[between(case_data_trim$age_at_start_of_pregnancy, 40,55)])

# calendar month of infection

cov_month<-months(as.Date(case_data_trim$covid_date))
cov_year<-year(as.Date(case_data_trim$covid_date))

cov_calendar_month_total<-paste0(cov_month, "-", cov_year)

covid_month_cases<-table(factor(cov_calendar_month_total, levels=levels_calendar))

total_cases<-c(number_preg, number_moms, gest_age_at_cov_median, gest_age_at_cov_IQR,
               age_at_start_preg_median, age_IQR, age_12_24, age_25_39, age_40_55)
##########################################################################################
# SEVERE CASES 

case_data_trim_severe<-case_data_trim[case_data_trim$severity==1,]

number_preg<-as.numeric(nrow(case_data_trim_severe))
number_moms<-as.numeric(length(unique(case_data_trim_severe$person_id)))

gest_age_at_cov_median<- round( median((case_data_trim_severe$gest_age_cov)/7),1)
gest_age_at_cov_IQR<- round(IQR((case_data_trim_severe$gest_age_cov)/7),1)

age_at_start_preg_median<-round(median(case_data_trim_severe$age_at_start_of_pregnancy),1)
age_IQR<-round(IQR(case_data_trim_severe$age_at_start_of_pregnancy),1)

# between includes min and max in the range >= and <=
age_12_24<-length(case_data_trim_severe$age_at_start_of_pregnancy[between(case_data_trim_severe$age_at_start_of_pregnancy, 12,24)])
age_25_39<-length(case_data_trim_severe$age_at_start_of_pregnancy[between(case_data_trim_severe$age_at_start_of_pregnancy, 25,39)])
age_40_55<-length(case_data_trim_severe$age_at_start_of_pregnancy[between(case_data_trim_severe$age_at_start_of_pregnancy, 40,55)])

# calendar month of infection

cov_month<-months(as.Date(case_data_trim_severe$covid_date))
cov_year<-year(as.Date(case_data_trim_severe$covid_date))

cov_calendar_month_total<-paste0(cov_month, "-", cov_year)

covid_month_severe<-table(factor(cov_calendar_month_total, levels=levels_calendar))

severe_cases<-c(number_preg, number_moms, gest_age_at_cov_median, gest_age_at_cov_IQR,
               age_at_start_preg_median, age_IQR, age_12_24, age_25_39, age_40_55)
##########################################################################################
# NON SEVERE CASES 

case_data_trim_nonsevere<-case_data_trim[case_data_trim$severity==0,]

number_preg<-as.numeric(nrow(case_data_trim_nonsevere))
number_moms<-as.numeric(length(unique(case_data_trim_nonsevere$person_id)))

gest_age_at_cov_median<- round( median((case_data_trim_nonsevere$gest_age_cov)/7),1)
gest_age_at_cov_IQR<- round(IQR((case_data_trim_nonsevere$gest_age_cov)/7),1)

age_at_start_preg_median<-round(median(case_data_trim_nonsevere$age_at_start_of_pregnancy),1)
age_IQR<-round(IQR(case_data_trim_nonsevere$age_at_start_of_pregnancy),1)

# between includes min and max in the range >= and <=
age_12_24<-length(case_data_trim_nonsevere$age_at_start_of_pregnancy[between(case_data_trim_nonsevere$age_at_start_of_pregnancy, 12,24)])
age_25_39<-length(case_data_trim_nonsevere$age_at_start_of_pregnancy[between(case_data_trim_nonsevere$age_at_start_of_pregnancy, 25,39)])
age_40_55<-length(case_data_trim_nonsevere$age_at_start_of_pregnancy[between(case_data_trim_nonsevere$age_at_start_of_pregnancy, 40,55)])

# calendar month of infection

cov_month<-months(as.Date(case_data_trim_nonsevere$covid_date))
cov_year<-year(as.Date(case_data_trim_nonsevere$covid_date))

cov_calendar_month_total<-paste0(cov_month, "-", cov_year)

covid_month_nonsevere<-table(factor(cov_calendar_month_total, levels=levels_calendar))

nonsevere_cases<-c(number_preg, number_moms, gest_age_at_cov_median, gest_age_at_cov_IQR,
               age_at_start_preg_median, age_IQR, age_12_24, age_25_39, age_40_55)

############################################################################################
# CONTROLS

number_preg<-as.numeric(nrow(control_data_trim))
number_moms<-as.numeric(length(unique(control_data_trim$person_id)))

gest_age_at_cov_median<- round( median((control_data_trim$gest_age_cov)/7),1)
gest_age_at_cov_IQR<- round(IQR((control_data_trim$gest_age_cov)/7),1)

age_at_start_preg_median<-round(median(control_data_trim$age_at_start_of_pregnancy),1)
age_IQR<-round(IQR(control_data_trim$age_at_start_of_pregnancy),1)

# between includes min and max in the range >= and <=
age_12_24<-length(control_data_trim$age_at_start_of_pregnancy[between(control_data_trim$age_at_start_of_pregnancy, 12,24)])
age_25_39<-length(control_data_trim$age_at_start_of_pregnancy[between(control_data_trim$age_at_start_of_pregnancy, 25,39)])
age_40_55<-length(control_data_trim$age_at_start_of_pregnancy[between(control_data_trim$age_at_start_of_pregnancy, 40,55)])

# calendar month of infection

cov_month<-months(as.Date(control_data_trim$cov_date))
cov_year<-year(as.Date(control_data_trim$cov_date))

cov_calendar_month_total<-paste0(cov_month, "-", cov_year)

covid_month_control<-table(factor(cov_calendar_month_total, levels=levels_calendar))

total_controls<-c(number_preg, number_moms, gest_age_at_cov_median, gest_age_at_cov_IQR,
               age_at_start_preg_median, age_IQR, age_12_24, age_25_39, age_40_55)

##########################################################################################
# SEVERE control 

control_data_trim_severe<-control_data_trim[control_data_trim$severe==1,]

number_preg<-as.numeric(nrow(control_data_trim_severe))
number_moms<-as.numeric(length(unique(control_data_trim_severe$person_id)))

gest_age_at_cov_median<- round( median((control_data_trim_severe$gest_age_cov)/7),1)
gest_age_at_cov_IQR<- round(IQR((control_data_trim_severe$gest_age_cov)/7),1)

age_at_start_preg_median<-round(median(control_data_trim_severe$age_at_start_of_pregnancy),1)
age_IQR<-round(IQR(control_data_trim_severe$age_at_start_of_pregnancy),1)

# between includes min and max in the range >= and <=
age_12_24<-length(control_data_trim_severe$age_at_start_of_pregnancy[between(control_data_trim_severe$age_at_start_of_pregnancy, 12,24)])
age_25_39<-length(control_data_trim_severe$age_at_start_of_pregnancy[between(control_data_trim_severe$age_at_start_of_pregnancy, 25,39)])
age_40_55<-length(control_data_trim_severe$age_at_start_of_pregnancy[between(control_data_trim_severe$age_at_start_of_pregnancy, 40,55)])

# calendar month of infection

cov_month<-months(as.Date(control_data_trim_severe$cov_date))
cov_year<-year(as.Date(control_data_trim_severe$cov_date))

cov_calendar_month_total<-paste0(cov_month, "-", cov_year)

covid_month_control_severe<-table(factor(cov_calendar_month_total, levels=levels_calendar))

severe_control<-c(number_preg, number_moms, gest_age_at_cov_median, gest_age_at_cov_IQR,
                age_at_start_preg_median, age_IQR, age_12_24, age_25_39, age_40_55)
##########################################################################################
# NON SEVERE controlS 

control_data_trim_nonsevere<-control_data_trim[control_data_trim$severe==0,]

number_preg<-as.numeric(nrow(control_data_trim_nonsevere))
number_moms<-as.numeric(length(unique(control_data_trim_nonsevere$person_id)))

gest_age_at_cov_median<- round( median((control_data_trim_nonsevere$gest_age_cov)/7),1)
gest_age_at_cov_IQR<- round(IQR((control_data_trim_nonsevere$gest_age_cov)/7),1)

age_at_start_preg_median<-round(median(control_data_trim_nonsevere$age_at_start_of_pregnancy),1)
age_IQR<-round(IQR(control_data_trim_nonsevere$age_at_start_of_pregnancy),1)

# between includes min and max in the range >= and <=
age_12_24<-length(control_data_trim_nonsevere$age_at_start_of_pregnancy[between(control_data_trim_nonsevere$age_at_start_of_pregnancy, 12,24)])
age_25_39<-length(control_data_trim_nonsevere$age_at_start_of_pregnancy[between(control_data_trim_nonsevere$age_at_start_of_pregnancy, 25,39)])
age_40_55<-length(control_data_trim_nonsevere$age_at_start_of_pregnancy[between(control_data_trim_nonsevere$age_at_start_of_pregnancy, 40,55)])

# calendar month of infection

cov_month<-months(as.Date(control_data_trim_nonsevere$cov_date))
cov_year<-year(as.Date(control_data_trim_nonsevere$cov_date))

cov_calendar_month_total<-paste0(cov_month, "-", cov_year)

covid_month_control_nonsevere<-table(factor(cov_calendar_month_total, levels=levels_calendar))

nonsevere_control<-c(number_preg, number_moms, gest_age_at_cov_median, gest_age_at_cov_IQR,
                   age_at_start_preg_median, age_IQR, age_12_24, age_25_39, age_40_55)

############################################################################################


#############################################################################################

baseline_rownames<-c("Number_of_pregnancies","Number_of_unique_mothers", "median_gestational_age_at_covid_infection",
                     "IQR_gestational_age_at_covid_infection","median_age_at_start_pregnancy",
                     "IQR_maternal_age","number_age_12_24","number_age_25_39","number_age_40_55")

baseline_output<-as.data.frame(cbind(baseline_rownames,total_cases, severe_cases, nonsevere_cases, total_controls, severe_control, nonsevere_control))

colnames(baseline_output)<-c("output", "all_cases", "severe_cases", "nonsevere_cases", "covid positive matched controls", "severe controls", "nonsevere controls")

baseline_covid_month<-as.data.frame(cbind(levels_calendar, as.numeric(covid_month_cases), as.numeric(covid_month_severe), as.numeric(covid_month_nonsevere),
                                          as.numeric(covid_month_control),as.numeric(covid_month_control_severe), as.numeric(covid_month_control_nonsevere)))
colnames(baseline_covid_month)<-c("output", "all_cases", "severe_cases", "nonsevere_cases", "covid positive matched controls", "severe controls", "nonsevere controls")
baseline_table<-rbind(baseline_output, baseline_covid_month)

fwrite(baseline_table, paste0(final_output_dir,DAP,"_table3", trimgroup[i], "_baseline.csv"))
}