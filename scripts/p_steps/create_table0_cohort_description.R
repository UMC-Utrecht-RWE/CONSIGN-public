
# cohort description table 


rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

# CHECK/ FILL IN YOUR PARAMETERS FOR PREGNANCY ALGORITHM FILE 

source("params.R")

source("99_path.R")

suppressMessages(source(paste0(pre_dir, "/packages.R")))


# Count of calender month of infection

calendar_months<-seq(pan_start_date,end_study_date,by="month")

pan_months<-months(calendar_months)
pan_years<-year(calendar_months)

levels_calendar<-paste0(pan_months, "-", pan_years)


my_row_names<-c("Count of pregnancies in the pandemic cohort (i.e. both covid-19 positive and negative pregnancies)", 
                "Median age of women /pregnancies in the pandemic cohort (i.e. both covid-19 positive and negative pregnancies)",
                "Count of pregnancies in the pandemic cohort with COVID-19 positive test during pregnancy (not trimester specific)",
                "Count of pregnancies with Covid-19 infection in Trimester 1",
                "Count of pregnancies with Covid-19 infection in Trimester 2",
                "Count of pregnancies with Covid-19 infection in Trimester 3",
                "Median gestational age of COVID-19 infection in weeks (not trimetser specific)",levels_calendar)

pan_preg_data<-fread(paste0(pan_preg_folder,"my_PREG.csv"))

pan_preg_cohort_num<-nrow(pan_preg_data)
median_age_pan_preg_cohort<-round(median(pan_preg_data$age_at_start_of_pregnancy),1)

cases_data<-fread(paste0(cov_pos_pan_preg_folder,"cov_pos_preg.csv"))
cases_preg_num<-nrow(cases_data)

cases_trim<-as.numeric(table(cases_data$cov_trimester))

trim1_cov<-cases_trim[1]
trim2_cov<-cases_trim[2]
trim3_cov<-cases_trim[3]

median_gest_age_cov<-round(median(cases_data$gest_age_cov/7),1)

cov_month<-months(as.Date(cases_data$covid_date))
cov_year<-year(as.Date(cases_data$covid_date))

cov_calendar_month_total<-paste0(cov_month, "-", cov_year)

covid_month_all<-table(factor(cov_calendar_month_total, levels=levels_calendar))

cohort_counts<-c(pan_preg_cohort_num, median_age_pan_preg_cohort, cases_preg_num,
                 trim1_cov, trim2_cov, trim3_cov, median_gest_age_cov,as.numeric(covid_month_all))

cohort_description<-as.data.frame(cbind(my_row_names, cohort_counts))

fwrite(cohort_description, paste0(final_output_dir, "cohort_description.csv"))
