cov_trimester<-function(preg_data=my_PREG){

cov_trim1<-rep(0, nrow(preg_data))
cov_trim2<-rep(0, nrow(preg_data))
cov_trim3<-rep(0, nrow(preg_data))

cov_trim1[preg_data$cov_date<=preg_data$trim_1_end]<-1
cov_trim2[(preg_data$cov_date>=preg_data$trim_2_start)&  (preg_data$cov_date<= preg_data$trim_2_end)]<-1
cov_trim3[(preg_data$cov_date>=preg_data$trim_3_start)&  (preg_data$cov_date<= preg_data$trim_3_end)]<-1

trim1<-preg_data[cov_trim1==1,c("person_id", "cov_date")]

trim2<-preg_data[cov_trim2==1,c("person_id", "cov_date")]

trim3<-preg_data[cov_trim3==1,c("person_id", "cov_date")]

split_trim_data<-list(trim1, trim2, trim3)
return(split_trim_data)}

trim_data<-cov_trimester(preg_data = my_PREG)
