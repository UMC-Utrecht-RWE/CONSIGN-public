#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#21/9/2022


#This script pulls drug exposure window results from trimester_drug_exposure.R 
# and transforms the data into table 1 (a,b,c) format
################################################################
CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

my_tables<-list.files(path=output_cov_window_atc_2)
my_names<-str_sub(unlist(my_tables), 1, str_length(unlist(my_tables))-22)

# denominator is all covid+ pregnancies by trimester

my_PREG<-fread(paste0(cov_pos_pan_preg_folder, "cov_pos_preg.csv"))

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
  my_data<-fread(paste0(output_cov_window_atc_2,my_tables[i]))
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

table_1a<-as.data.frame(cbind((my_names), rep(denom_trim_1_all, length(my_names)),(num_minus_1),(num_plus_1), 
                              rep(denom_trim_2_all, length(my_names)),(num_minus_2),(num_plus_2), 
                              rep(denom_trim_3_all, length(my_names)),(num_minus_3), (num_plus_3)))

colnames(table_1a)<-c("Drug", "trimester 1 denominator","trim 1, -30 days", "trim 1, +30 days", 
                      "trimester2 denominator", "trim 2, -30 days", "trim 2, +30 days",
                      "trimester 3 denominator", "trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1a, paste0(meta_dir,DAP, "_raw_table_1_a.csv"))

###################################################
#NON SEVERE
my_PREG_NS<-my_PREG[my_PREG$severity==0,]
denom_trim_1_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==1,])
denom_trim_2_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==2,])
denom_trim_3_NS<-nrow(my_PREG_NS[my_PREG_NS$cov_trimester==3,])


for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_cov_window_atc_2,my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  my_data<-my_data[my_data$severity=="0"]
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

table_1b<-as.data.frame(cbind((my_names), rep(denom_trim_1_NS, length(my_names)),(num_minus_1),(num_plus_1), 
                              rep(denom_trim_2_NS, length(my_names)),(num_minus_2),(num_plus_2), 
                              rep(denom_trim_3_NS, length(my_names)),(num_minus_3), (num_plus_3)))

colnames(table_1b)<-c("Drug", "NS trimester 1 denominator","trim 1, -30 days", "trim 1, +30 days", 
                      "NS trimester2 denominator", "trim 2, -30 days", "trim 2, +30 days",
                      "NS trimester 3 denominator", "trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1b, paste0(meta_dir,DAP, "_raw_table_1_b_nonsevere.csv"))


###################################################
#SEVERE
my_PREG_S<-my_PREG[my_PREG$severity==1,]
denom_trim_1_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==1,])
denom_trim_2_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==2,])
denom_trim_3_S<-nrow(my_PREG_S[my_PREG_S$cov_trimester==3,])


for(i in 1:length(my_tables)){
  my_data<-fread(paste0(output_cov_window_atc_2,my_tables[i]))
  my_data<-my_data[complete.cases(my_data)==T,]
  my_data<-my_data[my_data$severity=="1"]
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

table_1c<-as.data.frame(cbind((my_names), rep(denom_trim_1_S, length(my_names)),(num_minus_1),(num_plus_1), 
                              rep(denom_trim_2_S, length(my_names)),(num_minus_2),(num_plus_2), 
                              rep(denom_trim_3_S, length(my_names)),(num_minus_3), (num_plus_3)))

colnames(table_1c)<-c("Drug", "S trimester 1 denominator","trim 1, -30 days", "trim 1, +30 days", 
                      "S trimester2 denominator", "trim 2, -30 days", "trim 2, +30 days",
                      "S trimester 3 denominator", "trim 3, -30 days", "trim 3, +30 days")
fwrite(table_1c, paste0(meta_dir,DAP, "_raw_table_1_c_severe.csv"))


