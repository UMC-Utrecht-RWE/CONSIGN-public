#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#30/5/2022

#CONSIGN

#compare dates of medicine use with COVID diagnosis dates

actual_tables<-list()
actual_tables<-list.files(paste0(output_drugs,"/"))

# covid<-load dataset with covid diagnosis dates

# subset the ATC files-->
# pregnant+covid --> dates of drug dispensing + date of pregnancy trimesters 
# pregnant NO covid
# non pregnant +covid --> dates of drug dispensing
# non pregant NO covid

