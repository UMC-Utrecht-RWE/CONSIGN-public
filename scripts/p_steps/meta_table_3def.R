#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#7/6/2023


#This script pulls drug exposure window results from trimester_drug_exposure.R 
# and transforms the data into table 1 (a,b,c) format
################################################################
CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

my_tables<-list.files(path=output_cov_window_atc_2)
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-22)

# denominator is all covid+ pregnancies by trimester

my_PREG<-fread(paste0(matched_folder,"matches_cov_pos_non_preg.csv"))

denom_trim_1_all<-nrow(my_PREG[my_PREG$cov_trimester==1,])
denom_trim_2_all<-nrow(my_PREG[my_PREG$cov_trimester==2,])
denom_trim_3_all<-nrow(my_PREG[my_PREG$cov_trimester==3,])


#1a: all covid together

num_plus_1<-vector(length=length(my_tables))
num_plus_2<-vector(length=length(my_tables))
num_plus_3<-vector(length=length(my_tables))

num_minus_1<-vector(length=length(my_tables))
num_minus_2<-vector(length=length(my_tables))
num_minus_3<-vector(length=length(my_tables))


for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_CONTROL_NP_window_atc_2,my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
 
  num_minus_1[i]<-sum(as.numeric(my_data$minus_30[my_data$cov_trimester==1]))
  
  
  # TRIMESTER 2
  
  num_minus_2[i]<-sum(as.numeric(my_data$minus_30[my_data$cov_trimester==2]))
  
  
  # TRIMESTER 3
  
  num_minus_3[i]<-sum(as.numeric(my_data$minus_30[my_data$cov_trimester==3]))
  
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
 
  num_plus_1[i]<-sum(as.numeric(my_data$plus_30[my_data$cov_trimester==1]))
  
  
  # TRIMESTER 2
  
  num_plus_2[i]<-sum(as.numeric(my_data$plus_30[my_data$cov_trimester==2]))
  
  
  # TRIMESTER 3
  
  num_plus_3[i]<-sum(as.numeric(my_data$plus_30[my_data$cov_trimester==3]))
  
}

table_3def<-as.data.frame(cbind((my_names), rep(denom_trim_1_all, length(my_names)),(num_minus_1),(num_plus_1), 
                              rep(denom_trim_2_all, length(my_names)),(num_minus_2),(num_plus_2), 
                              rep(denom_trim_3_all, length(my_names)),(num_minus_3), (num_plus_3)))


colnames(table_3def)<-c("Drug", "trimester 1 denominator"," -30 days (controls)", "+30 days (controls)", 
                        "trimester 2 denominator","-30 days (controls)", "+30 days (controls)",
                        "trimester 3 denominator","-30 days (controls)", "+30 days (controls)")



fwrite(table_3def, paste0(meta_dir,DAP, "_raw_table_3def_total.csv"))

###############################################################################
#1b" non-severe


my_PREG_NS<-my_PREG[my_PREG$severity==0,]
denom_trim_1_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==1,])
denom_trim_2_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==2,])
denom_trim_3_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==3,])

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_CONTROL_NP_window_atc_2,my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  my_data<-my_data[my_data$severity=="0"]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
 
    num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
    
  
  # TRIMESTER 2
  
    num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
    
  # TRIMESTER 3
  
    num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
    
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
 
    num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
    
  
  # TRIMESTER 2
 
    num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
    
  # TRIMESTER 3
 
    num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
}
    

table_3def<-as.data.frame(cbind((my_names), rep(denom_trim_1_NS, length(my_names)),(num_minus_1),(num_plus_1), 
                                rep(denom_trim_2_NS, length(my_names)),(num_minus_2),(num_plus_2), 
                                rep(denom_trim_3_NS, length(my_names)),(num_minus_3), (num_plus_3)))


colnames(table_3def)<-c("Drug", "NS trimester 1 denominator"," -30 days (controls)", "+30 days (controls)", 
                        "NS trimester 2 denominator","-30 days (controls)", "+30 days (controls)",
                        "NS trimester 3 denominator","-30 days (controls)", "+30 days (controls)")


fwrite(table_3def, paste0(meta_dir,DAP, "_raw_table_3def_nonsevere.csv"))
#############################################################################################
#1b" non-severe


my_PREG_S<-my_PREG[my_PREG$severity==1,]
denom_trim_1_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==1,])
denom_trim_2_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==2,])
denom_trim_3_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==3,])

for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_CONTROL_NP_window_atc_2,my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  my_data<-my_data[my_data$severity=="1"]
  print(my_names[[i]])
  # 30 days BEFORE COVID infection date
  
  # TRIMESTER 1
  
  num_minus_1<-sum(my_data$minus_30[my_data$cov_trimester==1])
  
  
  # TRIMESTER 2
  
  num_minus_2<-sum(my_data$minus_30[my_data$cov_trimester==2])
  
  # TRIMESTER 3
  
  num_minus_3<-sum(my_data$minus_30[my_data$cov_trimester==3])
  
  # 30 days AFTER COVID infection date
  
  # TRIMESTER 1
  
  num_plus_1<-sum(my_data$plus_30[my_data$cov_trimester==1])
  
  
  # TRIMESTER 2
  
  num_plus_2<-sum(my_data$plus_30[my_data$cov_trimester==2])
  
  # TRIMESTER 3
  
  num_plus_3<-sum(my_data$plus_30[my_data$cov_trimester==3])
}


table_3def<-as.data.frame(cbind((my_names), rep(denom_trim_1_S, length(my_names)),(num_minus_1),(num_plus_1), 
                                rep(denom_trim_2_S, length(my_names)),(num_minus_2),(num_plus_2), 
                                rep(denom_trim_3_S, length(my_names)),(num_minus_3), (num_plus_3)))


colnames(table_3def)<-c("Drug", "S trimester 1 denominator"," -30 days (controls)", "+30 days (controls)", 
                        "S trimester 2 denominator","-30 days (controls)", "+30 days (controls)",
                        "S trimester 3 denominator","-30 days (controls)", "+30 days (controls)")


fwrite(table_3def, paste0(meta_dir,DAP, "_raw_table_3def_severe.csv"))