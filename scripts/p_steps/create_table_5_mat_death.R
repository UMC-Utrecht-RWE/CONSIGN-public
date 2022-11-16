# table 5 maternal death


# Number of maternal deaths in pregnancies with a trimester 1 infection, n
# Number of pregnancies with a trimester 1 infection, N
# Prevalence of maternal deaths in pregnancies with a trimester 1 infection, % (95% CI)

maternal_death_output<-as.data.frame(matrix(ncol=5, nrow=9))

colnames(maternal_death_output)<-c("names", "all cases", "severe cases", "non severe cases", "controls")    

my_rows<-c("# deaths trim1", "# preg trim1", "% trim1","# deaths trim2", "# preg trim2", "% trim2", "# deaths trim3", "# preg trim3", "% trim3" )
   
maternal_death_output$names<-my_rows

my_vars<-colnames(case_mat_outcome[,4:(ncol(case_mat_outcome)-5)])


                                  
trim1<-list()

trim1[[1]]<-sum(T1_case_mat_outcome$maternal_death)
trim1[[2]]<-length(T1_case_mat_outcome$maternal_death)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")

maternal_death_output[1:3,2]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_case_mat_outcome$maternal_death)
trim2[[2]]<-length(T2_case_mat_outcome$maternal_death)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")

maternal_death_output[4:6,2]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_case_mat_outcome$maternal_death)
trim3[[2]]<-length(T3_case_mat_outcome$maternal_death)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")

maternal_death_output[7:9,2]<-unlist(trim3)

###################################################################################

trim1<-list()

trim1[[1]]<-sum(T1_S_case_mat_outcome$maternal_death)
trim1[[2]]<-length(T1_S_case_mat_outcome$maternal_death)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")

maternal_death_output[1:3,3]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_S_case_mat_outcome$maternal_death)
trim2[[2]]<-length(T2_S_case_mat_outcome$maternal_death)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")

maternal_death_output[4:6,3]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_S_case_mat_outcome$maternal_death)
trim3[[2]]<-length(T3_S_case_mat_outcome$maternal_death)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")

maternal_death_output[7:9,3]<-unlist(trim3)



###################################################################################

trim1<-list()

trim1[[1]]<-sum(T1_NS_case_mat_outcome$maternal_death)
trim1[[2]]<-length(T1_NS_case_mat_outcome$maternal_death)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")

maternal_death_output[1:3,4]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_NS_case_mat_outcome$maternal_death)
trim2[[2]]<-length(T2_NS_case_mat_outcome$maternal_death)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")

maternal_death_output[4:6,4]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_NS_case_mat_outcome$maternal_death)
trim3[[2]]<-length(T3_NS_case_mat_outcome$maternal_death)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")

maternal_death_output[7:9,4]<-unlist(trim3)

################################################

trim1<-list()

trim1[[1]]<-sum(T1_control_mat_outcome$maternal_death)
trim1[[2]]<-length(T1_control_mat_outcome$maternal_death)
trim1_prop<-prop.test(trim1[[1]], trim1[[2]])
trim1[[3]]<-paste0((round(trim1_prop$estimate,5)*100)," (", (round(trim1_prop$conf.int[1],5)*100),"-",(round(trim1_prop$conf.int[2],5)*100),")")

maternal_death_output[1:3,5]<-unlist(trim1)

trim2<-list()

trim2[[1]]<-sum(T2_control_mat_outcome$maternal_death)
trim2[[2]]<-length(T2_control_mat_outcome$maternal_death)
trim2_prop<-prop.test(trim2[[1]], trim2[[2]])
trim2[[3]]<-paste0((round(trim2_prop$estimate,5)*100)," (", (round(trim2_prop$conf.int[1],5)*100),"-",(round(trim2_prop$conf.int[2],5)*100),")")

maternal_death_output[4:6,5]<-unlist(trim2)

trim3<-list()

trim3[[1]]<-sum(T3_control_mat_outcome$maternal_death)
trim3[[2]]<-length(T3_control_mat_outcome$maternal_death)
trim3_prop<-prop.test(trim3[[1]], trim3[[2]])
trim3[[3]]<-paste0((round(trim3_prop$estimate,5)*100)," (", (round(trim3_prop$conf.int[1],5)*100),"-",(round(trim3_prop$conf.int[2],5)*100),")")

maternal_death_output[7:9,5]<-unlist(trim3)

fwrite(maternal_death_output, paste0(final_output_dir,"table_5_maternal_death.csv"))



