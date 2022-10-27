#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#24/10/2022

# CREATE BASELINE CHARACTERISTICS OF CASES

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

pan_months<-months(calendar_months)
pan_years<-year(calendar_months)

levels_calendar<-paste0(pan_months, "-", pan_years)

trimgroup<-c("a","b","c")

for(i in 1:3){
case_data_trim<-case_data[case_data$cov_trimester==i]

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

covid_month_all<-table(factor(cov_calendar_month_total, levels=levels_calendar))

total_cases<-c(number_preg, number_moms, gest_age_at_cov_median, gest_age_at_cov_IQR,
               age_at_start_preg_median, age_IQR, age_12_24, age_25_39, age_40_55)


#############################################################################################

baseline_rownames<-c("Number_of_pregnancies","Number_of_unique_mothers", "median_gestational_age_at_covid_infection",
                     "IQR_gestational_age_at_covid_infection","median_age_at_start_pregnancy",
                     "IQR_maternal_age","number_age_12_24","number_age_25_39","number_age_40_55")

baseline_output<-as.data.frame(cbind(baseline_rownames,total_cases, severe_cases, nonsevere_cases))

colnames(baseline_output)<-c("output", "all_cases")

baseline_covid_month<-as.data.frame(cbind(levels_calendar, as.numeric(covid_month_all), as.numeric(covid_month_severe), as.numeric(covid_month_nonsevere)))
colnames(baseline_covid_month)<-c("output", "all_cases", "severe_cases", "nonsevere_cases")
baseline_table<-rbind(baseline_output, baseline_covid_month)

fwrite(baseline_table, paste0(final_output_dir,DAP,"_table2", trimgroup[i], "_baseline_cases.csv"))
}