
pan_preg_covid_negative <- fread(paste0(cov_neg_pan_preg_folder, "cov_neg_preg.csv"))

nrow(pan_preg_covid_negative)

cov_pos_pan_preg<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))

nrow(cov_pos_pan_preg)

all_pandemic_pregnancies<-fread(paste0(pan_preg_folder, "my_PREG.csv"))

nrow(all_pandemic_pregnancies)

