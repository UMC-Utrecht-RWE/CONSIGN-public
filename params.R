# USER INPUT 

# for 01_pregnancy_independent
#to use a pre-existing CDM location, first change set_my_CDM to TRUE
set_my_CDM<-F

#then fill in the my_CSM_path parameter

my_CDM_path<- "as_a_string/using_forward_slashes/location_of_your_CDM/"

# the exact name of YOUR pregnancy algorithm output, including file extension 
preg_data<-"imputed_pregnancy.csv"

# CHOOSE one of the following (csv OR rds) by commenting out (#) the format you are not using, and un-commenting the one you are using 
# if you have a different format, please change it to .csv OR .rds

preg_format<-"csv"

# preg_format<-"rds"

#######################################################################

# universal input

start_study_date<-as.Date(as.character("20180101"), format = "%Y%m%d")

end_study_date<-as.Date(as.character("20211231"), format = "%Y%m%d")

pan_start_date<-as.Date(as.character("20200301"), format = "%Y%m%d")

start_covariate_window<-as.Date(as.character("20190101"), format = "%Y%m%d")
