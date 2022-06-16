#set look_back parameter

################################################################

#test that date of drug dispensing is within trimester

my_PREG<-fread(paste0(hist_preg_folder,"my_PREG.csv"))

my_trimester<-"trimester_1"

my_trim_start<-"trim_1_start"
my_trim_end<-"trim_1_end"

my_tables<-list.files(path=output_drugs)

my_names<-c("Antihypertensives", "Antithrombotic","Antivirals","Antibacterials","Antimycotics","Antimycobacterials","Immune_sera_globulins",
            "Vaccinations","Analgesics","Psycholeptics", "Psychoanaleptics", "Diabetes", "Corticosteroids","Immunostimulants","Immunosuppressants",
            "Anti_inflammatory", "Nasal", "Cough_cold")

my_results<-list()

for(i in length(my_tables)){
my_data<-fread(paste0(output_drugs,my_tables[i]))
my_results[i]<-during_timester_test(expos_data=my_data, preg_data=my_PREG, trimester_start=my_trim_start, trimester_end= my_trim_end)

}



for (j in 1: length(my_results)){
  fwrite(A_group[j], paste0(output_trim, "/", "LB_",(-1*my_lookback), "_Mig_A",j,".csv"))
}

