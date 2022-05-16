# load D3_included_pregnancies  
load(paste0(dirtemp,"D3_pregnancy_with_risk.RData"))



### Creating trimesters
#Trimester 1: from Last Menstrual Period (LMP) to day < 98 after LMP; or end of pregnancy, whichever earlier
D3_preg_trim <- D3_pregnancy_with_risk[, trim1_start := pregnancy_start_date]

D3_preg_trim <- D3_preg_trim[pregnancy_end_date > (trim1_start + 97), 
                                               trim1_end := (trim1_start + 97)]

D3_preg_trim <- D3_preg_trim[pregnancy_end_date <= (trim1_start + 97), 
                                               trim1_end := pregnancy_end_date]

# Trimester 2: from day 98 after LMP to day  <196 after LMP; or end of pregnancy, whichever earlier
D3_preg_trim <- D3_preg_trim[pregnancy_end_date > trim1_end,
                                               trim2_start := trim1_end + 1 ]

D3_preg_trim <- D3_preg_trim[pregnancy_end_date > (trim1_start + 195), 
                                               trim2_end := (trim1_start + 195)]

D3_preg_trim <- D3_preg_trim[pregnancy_end_date <= (trim1_start + 195) & pregnancy_end_date > trim1_end, 
                             trim2_end := pregnancy_end_date]

# Trimester 3: from day 196 after LMP onwards until end of pregnancy
D3_preg_trim <- D3_preg_trim[pregnancy_end_date > trim2_end,
                             `:=` (trim3_start = trim2_end + 1,  trim3_end = pregnancy_end_date)]

# create binary variable for pregnancy that ends after 31/12/2020
D3_preg_trim <- D3_preg_trim[pregnancy_end_date  >= as.Date("2020-12-31"), end_preg_in_2021 := 1][is.na(end_preg_in_2021), end_preg_in_2021 := 0]

# adding age 
load(paste0(dirtemp, "D3_PERSONS.RData"))
D3_preg_trim <- merge(D3_preg_trim, D3_PERSONS[,.(person_id, date_of_birth)], 
                        all.x = T, by = "person_id")

D3_preg_trim <- D3_preg_trim[, age:= as.integer((pregnancy_start_date - date_of_birth) / 365)]
D3_preg_trim <- D3_preg_trim[, -c("date_of_birth")]

D3_preg_trim <- D3_preg_trim[age>=12 & age<=34, age_category := 1]
D3_preg_trim <- D3_preg_trim[age>=35 & age<=55, age_category := 2]
#D3_preg_trim_2[is.na(age_category)] 

## sorting by casual order
D3_preg_trim <- D3_preg_trim[, casual_number := sample(seq(1, nrow(D3_preg_trim)), nrow(D3_preg_trim), replace = F)]
D3_preg_trim <- D3_preg_trim[order(casual_number)]
D3_preg_trim <- D3_preg_trim[, -c("casual_number")]

# Melting 
D3_preg_trim_2 = melt(D3_preg_trim, 
                              id.vars = c("person_id", "pers_group_id", 
                                          "survey_id", "highest_quality", 
                                          "PROMPT", "pregnancy_start_date",
                                          "pregnancy_end_date", "type_of_pregnancy_end",
                                          "at_risk", "end_preg_in_2021", "age", "age_category"),
                              measure.vars = list(c("trim1_start", "trim2_start", "trim3_start"), 
                                               c("trim1_end", "trim2_end", "trim3_end")))
setnames(D3_preg_trim_2, 
         c("variable", "value1", "value2"),
         c("trim", "start_trim", "end_trim"))

D3_preg_trim_2 <- D3_preg_trim_2[!is.na(start_trim)]

# delete trimesters that starts after 31/12/2020
D3_preg_trim_2 <- D3_preg_trim_2[!(start_trim>= as.Date("2020-12-31")),]

# create binary variable for pregnancy trimesters that ends after 31/12/2020
D3_preg_trim_2 <- D3_preg_trim_2[end_trim >= as.Date("2020-12-31"), end_trim_in_2021 := 1][is.na(end_trim_in_2021), end_trim_in_2021 := 0]

# create "prior_to_COVID": 0 if end_trim > 1 march 2020
D3_preg_trim_2 <- D3_preg_trim_2[end_trim >= as.Date("2020-03-01"),
                                                 prior_to_COVID:=0][is.na(prior_to_COVID), prior_to_COVID:=1]


# creating covid registry 
SURVEY_ID_COVID <- data.table()
files<-sub('\\.csv$', '', list.files(dirinput))
for (i in 1:length(files)) {
  if (str_detect(files[i],"^SURVEY_ID")) {
    SURVEY_ID_COVID <-rbind(SURVEY_ID_COVID,fread(paste0(dirinput,files[i],".csv"), colClasses = list( character="person_id"))[survey_meaning =="covid_registry",])
  }
}

covid_registry <- SURVEY_ID_COVID[,date:=ymd(survey_date)]
covid_registry <- covid_registry[,.(person_id, date)]

# merging covid in trimesters
MFC_covid_in_trim <- MergeFilterAndCollapse( listdatasetL = list(covid_registry),
                                             datasetS = D3_preg_trim_2, 
                                             typemerge = 2,
                                             key = c("person_id"), 
                                             condition = "date >= start_trim & date <= end_trim",
                                             strata=c("pers_group_id", "trim"),
                                             summarystat = list(list(c("exist"), "date" , "covid_in_trim")))

D3_preg_trim_3 <- merge(D3_preg_trim_2, MFC_covid_in_trim, by = c("pers_group_id", "trim"), all.x = T)
D3_preg_trim_3 <- D3_preg_trim_3[is.na(covid_in_trim), covid_in_trim := 0]
D3_preg_trim_3 <- merge(D3_preg_trim_3, covid_registry, by = "person_id", all.x = T)
D3_preg_trim_3 <- D3_preg_trim_3[!(covid_in_trim == 1), date := NA]
D3_pregnancy_risk_trimester <- D3_preg_trim_3

# create variable "status"
D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[, covid_in_preg := max(covid_in_trim), by = "pers_group_id"]
D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[covid_in_trim == 1, trimester_covid := as.integer(trim)]

D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[is.na(trimester_covid), trimester_covid := 0]

#D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[, trimester_covid := as.integer(trimester_covid)]
D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[, trimester_covid := max(trimester_covid), by = "pers_group_id"]

D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[covid_in_trim == 1, status := 1]
D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[covid_in_preg == 1 & covid_in_trim == 0 & (trimester_covid <  as.integer(trim)), status := 2]
D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[is.na(status) & prior_to_COVID == 0, status := 0]
D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[prior_to_COVID == 1, status := NA]

#View(D3_pregnancy_risk_trimester[, .(covid_in_preg, pers_group_id, trim, covid_in_trim, status,trimester_covid )][order(covid_in_preg, pers_group_id, trim )])
D3_pregnancy_risk_trimester <- D3_pregnancy_risk_trimester[, -c("covid_in_preg", "trimester_covid" )]
save(D3_pregnancy_risk_trimester, file=paste0(dirtemp,"D3_pregnancy_risk_trimester.RData"))

rm(D3_pregnancy_risk_trimester, D3_preg_trim_3, D3_preg_trim_2, D3_preg_trim,
   D3_pregnancy_with_risk, MFC_covid_in_trim, covid_registry, SURVEY_ID_COVID,
   D3_PERSONS)
