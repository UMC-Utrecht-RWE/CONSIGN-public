#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 14/7/2022


# create trimester start and end dates


load(paste0(pathCDM,"D3_pregnancy_final.RData"))

my_PREG<-D3_pregnancy_final

my_PREG$premature<-0

my_PREG$trim_1_start<- my_PREG$pregnancy_start_date
my_PREG$trim_1_end<- my_PREG$pregnancy_start_date+97
my_PREG$trim_1_end[my_PREG$trim_1_end>=my_PREG$pregnancy_end_date]<-my_PREG$pregnancy_end_date
my_PREG$premature[my_PREG$trim_1_end>=my_PREG$pregnancy_end_date]<-1

my_PREG$trim_2_start<-0
my_PREG$trim_2_start[my_PREG$premature==1]<-NA
my_PREG$trim_2_start[my_PREG$premature==0]<-my_PREG$trim_1_end+1

my_PREG$trim_2_end<- my_PREG$trim_1_end+97
my_PREG$trim_2_end[my_PREG$trim_2_end>=my_PREG$pregnancy_end_date]<-my_PREG$pregnancy_end_date
my_PREG$premature[my_PREG$trim_2_end>=my_PREG$pregnancy_end_date]<-1

my_PREG$trim_3_start<-0
my_PREG$trim_3_start[my_PREG$premature==1]<-NA
my_PREG$trim_3_start[my_PREG$premature==0]<-my_PREG$trim_2_end+1

my_PREG$trim_3_end<- my_PREG$pregnancy_end_date
my_PREG$trim_3_end[is.na(my_PREG$trim_3_start)]<- NA
my_PREG$trim_3_end[my_PREG$trim_3_end<my_PREG$trim_2_end]<-NA
