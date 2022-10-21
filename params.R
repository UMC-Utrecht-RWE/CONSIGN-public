# USER INPUT 

# for 01_to_run_pregnancy_independent
#to use a pre-existing CDM location, first change set_my_CDM to TRUE
set_my_CDM<-F

#then fill in the my_path_CDM parameter
# IMPORTANT: USE / BACKSLASH (NOT FORWARD SLASH \ ) IF YOU COPY AND PASTE YOUR PATH, YOU WILL PROBABLY NEED TO REPLACE \ WITH / MANUALLY
# IMPORTANT: END THE STRING WITH /

my_path_CDM<- "a_text_string/showing_where/your_CDM/is_located/"

# for 02_to_run_pregnancy_dependent

# specify your pregnancy path "a_text_string/showing_where/your_CDM/is_located/" using backslash and ending in backslash
# if pregnancy file stored with CDM, preg_path<-my_path_CDM

preg_path<- "C:/Users/Gebruiker/OneDrive/Documents/projects/CONSIGN/CDMInstances_preselect/"

# the exact name of YOUR pregnancy algorithm output, including file extension 
preg_data<-"imputed_pregnancy.csv"

# CHOOSE one of the following (csv OR rds) by commenting out (#) the format you are not using, and un-commenting the one you are using 
# if you have a different format, please change it to .csv OR .rds

preg_format<-"csv"

# preg_format<-".RData"

#######################################################################

# universal input

# minimum age = 12 
# maximum age = 55

# Age groups will be defined based upon age at the start date of the pregnancy:
# 12-34 years of age
# 35-55 years of age

start_study_date<-as.Date(as.character("20180101"), format = "%Y%m%d")

end_study_date<-as.Date(as.character("20211231"), format = "%Y%m%d")

pan_start_date<-as.Date(as.character("20200301"), format = "%Y%m%d")

start_covariate_window<-as.Date(as.character("20190101"), format = "%Y%m%d")

