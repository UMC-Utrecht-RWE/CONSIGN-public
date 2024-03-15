#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 12/8/2022
#update 18/10/22

# subset covid + and covid- pan_preg and non_preg

cov_data<-fread(paste0(preselect_folder, "/covid_data.csv"))

source(paste0(pre_dir,"/IMPORT_PATTERN_FUNC.R"))

'%exclude%' <- function(x,y)!('%in%'(x,y))

###############################################################

cov_preg_data<-fread(paste0(pan_preg_folder,"trim_cov_PREG.csv"))
cov_neg_pan_preg<-fread(paste0(cov_neg_pan_preg_folder,"cov_neg_preg.csv"))

# check for days to deliver at covid date and remove those within 3 days
# keep all cov_pos_preg, but overwrite severity if cov_date is within 

cov_preg_data$severity[cov_preg_data$days_cov_before_labor<3]<-9
table(cov_preg_data$severity)
cov_preg_at_delivery<-cov_preg_data[cov_preg_data$days_cov_before_labor<=3,]
fwrite(cov_preg_at_delivery, paste0(output_dir,"covid_within_3_days_delivery.csv"))


all_pan_preg<-fread(paste0(pan_preg_folder,"my_PREG.csv"))

  
pan_tables<-list.files(paste0(pan_preg_folder,"/"), pattern = "\\.csv$")


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
  # unlink(paste0(pan_preg_folder,pan_tables[i]))
}



unlink(paste0(cov_neg_pan_preg_folder,"my_PREG.csv"))
unlink(paste0(cov_neg_pan_preg_folder,"trim_cov_PREG.csv"))


unlink(paste0(cov_pos_pan_preg_folder,"my_PREG.csv"))
unlink(paste0(cov_pos_pan_preg_folder,"trim_cov_PREG.csv"))


##############################################################################################


not_preg_tables<-list.files(paste0(not_preg_folder,"/"), pattern = "\\.csv$")

not_preg_PERSONS<-IMPORT_PATTERN(pat="PERSONS", dir=not_preg_folder)

cov_pos_not_preg<-not_preg_PERSONS[not_preg_PERSONS$person_id%in%cov_data$person_id,]

cov_data_pos_not_preg<-cov_data[cov_data$person_id%in%not_preg_PERSONS$person_id,]

# need first cov_date for matching

covid_grouped<-cov_data_pos_not_preg%>%group_by(person_id)

first_covid<-covid_grouped%>%slice_min( n = 1, order_by = cov_date)
# it is possible that 2 covid events are observed on the same day, so they would be repeated
#add unique statement
first_covid<-unique(first_covid)

fwrite(first_covid, paste0(cov_pos_not_preg_folder, "covid_data_not_preg.csv"))

for (i in 1:length(not_preg_tables)){
  my_table<-fread(paste0(not_preg_folder,not_preg_tables[i]))
  my_preg_table<- my_table[my_table$person_id%in%cov_pos_not_preg$person_id,]
  fwrite(my_preg_table,paste0(cov_pos_not_preg_folder,not_preg_tables[i]))
  
}
# 
# for (i in 1:length(not_preg_tables)){
#   my_table<-fread(paste0(not_preg_folder,not_preg_tables[i]))
#   my_preg_table<- my_table[my_table$person_id%exclude%cov_pos_not_preg$person_id,]
#   fwrite(my_preg_table,paste0(cov_neg_not_preg_folder,not_preg_tables[i]))
#   # unlink(paste0(not_preg_folder,not_preg_tables[i]))
# }

