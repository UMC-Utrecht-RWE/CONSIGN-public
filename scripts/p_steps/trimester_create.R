#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 14/7/2022


# create trimester start and end dates


my_PREG<-fread(paste0(path_CDM,preg_data))

summary(my_PREG$pregnancy_start_date)

my_PREG$pregnancy_start_date<-as.numeric(as.Date(my_PREG$pregnancy_start_date, format = "%m/%d/%Y"))
my_PREG$pregnancy_end_date<-as.numeric(as.Date(my_PREG$pregnancy_end_date, format = "%m/%d/%Y"))

my_PREG<-my_PREG[!is.na(my_PREG$pregnancy_start_date),]


my_PREG$trim_1_start<- my_PREG$pregnancy_start_date
my_PREG$trim_1_end<- my_PREG$pregnancy_start_date+97


my_PREG$trim_2_start<-(my_PREG$trim_1_end)+1
my_PREG$trim_2_end<- my_PREG$trim_2_start+97


my_PREG$trim_3_start<-my_PREG$trim_2_end+1
my_PREG$trim_3_end<- my_PREG$pregnancy_end_date


my_PREG$trim_1_end[my_PREG$trim_1_end>=my_PREG$pregnancy_end_date]<-my_PREG$pregnancy_end_date[my_PREG$trim_1_end>=my_PREG$pregnancy_end_date]

my_PREG$trim_2_start[my_PREG$trim_1_end==my_PREG$pregnancy_end_date]<-0
my_PREG$trim_2_end[my_PREG$trim_2_start==0]<-0
my_PREG$trim_2_end[my_PREG$trim_2_end>=my_PREG$pregnancy_end_date]<-my_PREG$pregnancy_end_date[my_PREG$trim_2_end>=my_PREG$pregnancy_end_date]

my_PREG$trim_3_start[my_PREG$trim_2_end==my_PREG$pregnancy_end_date]<-0
my_PREG$trim_3_start[my_PREG$trim_2_end==0]<-0
my_PREG$trim_3_end[my_PREG$trim_3_start==0]<-0

my_PREG$trim_2_start[my_PREG$trim_2_start==0]<-NA
my_PREG$trim_2_end[my_PREG$trim_2_end==0]<-NA
my_PREG$trim_3_start[my_PREG$trim_3_start==0]<-NA
my_PREG$trim_3_end[my_PREG$trim_3_end==0]<-NA

fwrite(my_PREG, paste0(path_CDM, "preg_trim.csv"))