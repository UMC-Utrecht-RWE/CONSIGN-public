#17/8/2023
#author Ema Alsina, M Sc Git: EmaUtrecht

#this script checks if all cases (pregnant with covid) are present
#in the matched cases at the person and pregnancy level and if they have
#been successfully matched with at least one covid+ control and/or one pregnant control

#the results are written to g_output/final/ 

#results SHOULD be 100% for all columns if matching procedure was successful
#EXCEPT for pregnancy ID, where in the case of multiple covid+ pregnancies,
#only the first is elligible to be matched

before_match_cases<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))
after_match_cases<- fread(paste0(matched_folder,"matches_cases.csv"))

covid_matches<-fread(paste0(matched_folder,"matched_covid_postive.csv"))
pregnant_matches<-fread(paste0(matched_folder,"matched_pregnant.csv"))


persons_same<-(before_match_cases$person_id%in% after_match_cases$person_id)
pregnancies_same<-(before_match_cases$pregnancy_id%in% after_match_cases$pregnancy_id)

has_covid_match<-(after_match_cases$person_id%in%covid_matches$exposed_id)
has_pregnant_match<-(after_match_cases$person_id%in%pregnant_matches$exposed_id)

matching_report<-(c((sum(persons_same)/length(persons_same)),
                  (sum(pregnancies_same)/length(pregnancies_same)),
                  (sum(has_covid_match)/length(has_covid_match)),
                  (sum(has_pregnant_match)/length(has_pregnant_match)))*100)
report_titles<-c("% person id in both cases and matched cases", "% pregnancy id in both cases and matched cases", 
                 "% matched cases with covid+ match", "% matched cases with pregnant match")
report<-as.data.frame(rbind(report_titles, matching_report))

fwrite(report, paste0(final_output_dir, "matching_procedure_report.csv"))
