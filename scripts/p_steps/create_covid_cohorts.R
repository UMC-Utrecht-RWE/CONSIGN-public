#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 12/8/2022


# subset covid + and covid- pan_preg and non_preg

cov_data<-fread(paste0(projectFolder, "/covid_data.csv"))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

'%exclude%' <- function(x,y)!('%in%'(x,y))

###############################################################

cov_preg_data<-fread(paste0(pan_preg_folder,"trim_cov_PREG.csv"))

all_pan_preg<-fread(paste0(pan_preg_folder,"my_PREG.csv"))

# HELP >_< split data by person_id OR preg_id? 

cov_neg_pan_preg<-all_pan_preg[all_pan_preg$person_id%exclude%cov_preg_data$person_id,]
  
pan_tables<-list.files(paste0(pan_preg_folder,"/"), pattern = "\\.csv$")

# pan_preg_PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=pan_preg_folder)
# 
# cov_pos_pan_preg<-pan_preg_PERSONS[pan_preg_PERSONS$person_id%in%cov_preg_data$person_id,]

for (i in 1:length(pan_tables)){
  my_table<-fread(paste0(pan_preg_folder,pan_tables[i]))
  my_preg_table<- my_table[my_table$person_id%in%cov_preg_data$person_id,]
  fwrite(my_preg_table,paste0(cov_pos_pan_preg_folder,pan_tables[i]))
}

fwrite(cov_preg_data, paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))

for (i in 1:length(pan_tables)){
  my_table<-fread(paste0(pan_preg_folder,pan_tables[i]))
  my_preg_table<- my_table[my_table$person_id%in%cov_neg_pan_preg$person_id,]
  fwrite(my_preg_table,paste0(cov_neg_pan_preg_folder,pan_tables[i]))
  unlink(paste0(pan_preg_folder,pan_tables[i]))
}

fwrite(cov_neg_pan_preg, paste0(cov_neg_pan_preg_folder, "cov_neg_preg.csv"))

##############################################################################################


not_preg_tables<-list.files(paste0(not_preg_folder,"/"), pattern = "\\.csv$")

not_preg_PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=not_preg_folder)

cov_pos_not_preg<-not_preg_PERSONS[not_preg_PERSONS$person_id%in%cov_data$person_id,]

for (i in 1:length(not_preg_tables)){
  my_table<-fread(paste0(not_preg_folder,not_preg_tables[i]))
  my_preg_table<- my_table[my_table$person_id%in%cov_pos_not_preg$person_id,]
  fwrite(my_preg_table,paste0(cov_pos_not_preg_folder,not_preg_tables[i]))
  
}

for (i in 1:length(not_preg_tables)){
  my_table<-fread(paste0(not_preg_folder,not_preg_tables[i]))
  my_preg_table<- my_table[my_table$person_id%exclude%cov_pos_not_preg$person_id,]
  fwrite(my_preg_table,paste0(cov_neg_not_preg_folder,not_preg_tables[i]))
  unlink(paste0(not_preg_folder,not_preg_tables[i]))
}

