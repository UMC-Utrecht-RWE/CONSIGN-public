#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/10/2022

# CONSIGN
# applied after pregnancy cohorts 

# Creates entry and exit dates for each person based on their elligibility and data availability
# Adds spell_start and spell_end

PERSONS<-fread(paste0(preselect_folder,"PERSONS.csv"))
OBS_SPELLS <- fread(paste0(preselect_folder,"ALL_OBS_SPELLS.csv"))
# copy columns
OBS_SPELLS$spell_start_date<-OBS_SPELLS$op_start_date
OBS_SPELLS$spell_end_date<-OBS_SPELLS$op_end_date
# Drops unnecessary columns
OBS_SPELLS<-OBS_SPELLS[,c("person_id","spell_start_date", "spell_end_date")]
OBS_SPELLS$spell_start_date<-as.numeric(OBS_SPELLS$spell_start_date)
OBS_SPELLS$spell_end_date<-as.numeric(OBS_SPELLS$spell_end_date)
OBS_SPELLS<-OBS_SPELLS[OBS_SPELLS$person_id%in%PERSONS$person_id,]
PERSONS<-PERSONS[PERSONS$person_id%in%OBS_SPELLS$person_id]


# missing day of birth? would this happen in real data? impute 15
PERSONS$comp_day_birth<-PERSONS$day_of_birth
PERSONS$comp_day_birth[is.na(PERSONS$day_of_birth)]<-15

DOB<-paste0(PERSONS$comp_day_birth, "/", PERSONS$month_of_birth, "/",PERSONS$year_of_birth)
PERSONS$DOB<-as.Date(DOB, format="%d/%m/%Y")
PERSONS$num_DOB<-as.numeric(PERSONS$DOB)

PERSONS$min_bday<-(PERSONS$num_DOB)+(4383) #12 years in days

# Creates Entry Date 
## entry date <- latest date at which ANY of the following conditions are met: age > 12, observation starts, study starts
# since we made the spell dates as most recent- may cause data loss on historical cohort
PERSONS$entry_date_historical<-pmax( PERSONS$min_bday, as.numeric(start_study_date))
PERSONS$entry_date_pandemic<-pmax( PERSONS$min_bday, as.numeric(pan_start_date))

# remove spell date from criteria

# day of death

DOD<-paste0(PERSONS$day_of_death, "/", PERSONS$month_of_death, "/",PERSONS$year_of_death)
PERSONS$DOD<-as.Date(DOD, format="%d/%m/%Y")
PERSONS$num_DOD<-as.numeric(PERSONS$DOD)

PERSONS$max_bday<-(PERSONS$num_DOB)+(20088) #55 years in days

PERSONS$exit_date<-pmin(OBS_SPELLS$spell_end_date, PERSONS$max_bday, as.numeric(end_study_date))


#COHORT ELLIGIIBILITY CANNOT INCLUDE EXIT DATES (COMPLETE OBSERVATION DURING STUDY PERIOD) BECASE WE NEED TO OBSERVE DEATH 
# historical elligible

PERSONS$elligible_historical<-0
PERSONS$elligible_historical[(PERSONS$entry_date_historical<= start_study_date)&(PERSONS$max_bday>=start_study_date)]<-1
table(PERSONS$elligible_historical)
mean(PERSONS$elligible_historical)
# pandemic elligible

PERSONS$elligible_pandemic<-0
PERSONS$elligible_pandemic[(PERSONS$entry_date_pandemic<=pan_start_date)&(PERSONS$max_bday>=pan_start_date)]<-1
table(PERSONS$elligible_pandemic)
mean(PERSONS$elligible_pandemic)

hist(PERSONS$age_at_start_pandemic[PERSONS$elligible_pandemic==0])
table(PERSONS$entry_date_pandemic[PERSONS$elligible_pandemic==0])
mean(PERSONS$elligible_pandemic)

# make age groups

PERSONS$age_at_start_study<-(as.numeric(start_study_date)-PERSONS$num_DOB)/365
PERSONS$age_at_start_study<-round(PERSONS$age_at_start_study,0)
# hist(PERSONS$age_at_start_study, breaks=50)
# abline(v=12, col=2, lwd=2)
# abline(v=55, col=2, lwd=2)



PERSONS$age_at_start_pandemic<-(as.numeric(pan_start_date)-PERSONS$num_DOB)/365
PERSONS$age_at_start_pandemic<-round(PERSONS$age_at_start_pandemic,0)
# hist(PERSONS$age_at_start_pandemic, breaks=50)
# abline(v=12, col=2, lwd=2)
# abline(v=55, col=2, lwd=2)

PERSONS$age_group<-PERSONS$age_at_start_pandemic

PERSONS$age_group[PERSONS$age_group<12]<- NA
PERSONS$age_group[PERSONS$age_group>55]<- NA

PERSONS$age_group[between(PERSONS$age_at_start_pandemic, 12,24)]<-1

PERSONS$age_group[between(PERSONS$age_at_start_pandemic, lower=25, upper=39)]<-2

PERSONS$age_group[between(PERSONS$age_at_start_pandemic,40,55)]<-3

table(PERSONS$age_group)


fwrite(PERSONS, paste0(preselect_folder,"PERSONS.csv"))


# table(PERSONS$age_at_start_pandemic[(PERSONS$entry_date_pandemic==18322)&PERSONS$elligible_pandemic==0])
# table(PERSONS$age_group[(PERSONS$entry_date_pandemic==18322)&PERSONS$elligible_pandemic==0])
