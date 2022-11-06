
standard_diff<-function(cohort1=mycohort1, cohort2=mycohort2){
 total_SD<-sd(c(mycohort1, mycohort2))
 print(total_SD)
 if(total_SD==0){result<-NA}else{
 prop1<-sum(mycohort1)/length(mycohort1)
 prop2<-sum(mycohort2)/length(mycohort2)
 prop_diff<-prop1-prop2
 result<-prop_diff/total_SD}
 return(result)
}

