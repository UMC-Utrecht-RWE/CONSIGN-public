#impute start and end dates in the pregnancy data to create "pandemic pregnancies" for script testing
#quick and dirty solution, long term need richer simulates data
#will likely cause data on perscriptions etc... to not make sense

df_preg<- fread(paste0(path_CDM, "pregnancy.csv"))

# summary(df_preg)

df_preg$pregnancy_start_date<-as.Date(df_preg$pregnancy_start_date, format = "%m/%d/%Y")
df_preg$pregnancy_end_date<-as.Date(df_preg$pregnancy_end_date, format = "%m/%d/%Y")


summary(df_preg)

# interesting! the data is so sparse because only 230 records have complete data for start_date
#sooo, I'll  impute these missing values 

to_replace<-sum(is.na(df_preg$pregnancy_start_date))

from_date<-as.Date("2021/06/01")-to_replace

new_start_dates<-seq(from_date, by = "day", length.out = to_replace)

new_end_dates<-new_start_dates+280

df_preg$pregnancy_start_date[is.na(df_preg$pregnancy_start_date)]<-new_start_dates
  
df_preg$pregnancy_end_date[is.na(df_preg$pregnancy_end_date)]<-new_end_dates

hist(df_preg$pregnancy_start_date, breaks=100)

df_preg$pregnancy_id<-1:nrow(df_preg)

df_preg$pregnancy_start_date

fwrite(df_preg, paste0(path_CDM, "imputed_pregnancy.csv"))
