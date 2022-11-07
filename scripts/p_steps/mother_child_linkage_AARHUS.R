#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#21/10

# identify children born from case and pregnant matched control group

# from pregnancy algorithm output: person_id=="related_id"=="birth_mother" 
# from pregnancy algorithm output: pregnancy_end_date== DOB child (in persons)

# need to source PERSONS from path_cdm because those under 12 were excluded
# only need ID and birth date

# need to add persons_relationship table path and name to params, or check if they are standard

'%exclude%' <- function(x,y)!('%in%'(x,y))


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

# number of days "persons date of birth" can vary from "end of pregnancy"
buffer<-30

PERSONS_RELATIONS<- fread(paste0(path_PR,PR_name))


PERSONS<-fread(paste0(path_CDM,"PERSONS.csv"), select = c("person_id", "day_of_birth", "month_of_birth", "year_of_birth"))

PERSONS_RELATIONS$person_id <- as.character(PERSONS_RELATIONS$person_id)
PERSONS$person_id <- as.character(PERSONS$person_id)

CHILDREN<-PERSONS[PERSONS$year_of_birth>2017,]
CHILDREN<-CHILDREN[duplicated(CHILDREN$person_id)==F,]

CHILDREN$comp_day_birth<-CHILDREN$day_of_birth
CHILDREN$comp_day_birth[is.na(CHILDREN$day_of_birth)]<-15

CHILDREN$comp_month_birth<-CHILDREN$month_of_birth
CHILDREN$comp_month_birth[is.na(CHILDREN$month_of_birth)]<-6

DOB<-paste0(CHILDREN$comp_day_birth, "/", CHILDREN$comp_month_birth, "/",CHILDREN$year_of_birth)
CHILDREN$DOB<-as.Date(DOB, format="%d/%m/%Y")
CHILDREN$DOB_numeric<-as.numeric(CHILDREN$DOB)

###########################################################
# bring in pregnancy data to find the children 

historical<-fread(paste0(hist_preg_folder, "my_PREG.csv"))



historical_mom_id<-historical$person_id[historical$type_of_pregnancy_end=="LB"]
historical_DOB<-historical$pregnancy_end_date[historical$type_of_pregnancy_end=="LB"]
historical_child<-list()
historical_child_DOB_PERSONS<-list()
hist_child_preg_DOB<-list()



##############################################################################

historical_PR<-PERSONS_RELATIONS[PERSONS_RELATIONS$related_id%in%historical_mom_id,]
# all children of historical mother == child_id == historical_PR$person_id
# check child_ids against DOB
historical_all_children<-CHILDREN[CHILDREN$person_id%in%historical_PR$person_id]

historical_all_children<-merge(historical_PR,historical_all_children, by="person_id")


# for each mom_id and historical_DOB combination, test DOB of CHILDREN with mom_id

# POSSIBLE that 1 mom has 2 historical pregnancies 
# that's fine- different DOB
# BUT what about TWINS? 

for(i in 1:length(historical_mom_id)){
  mom<- historical_mom_id[i]
  my_DOB<-historical_DOB[i]
  offspring<-historical_all_children[historical_all_children$related_id==mom,]
  offspring$days_to_DOB<- (offspring$DOB_numeric)-(my_DOB)
  print(offspring$days_to_DOB)
  historical_child[[i]]<-offspring$person_id[abs(offspring$days_to_DOB)<buffer]
  historical_child_DOB_PERSONS[[i]]<-offspring$DOB_numeric[abs(offspring$days_to_DOB)<buffer]
  hist_child_preg_DOB[[i]]<-rep(my_DOB, length(historical_child[[i]]))
}

historical_neonates<-as.data.frame(cbind(unlist(historical_child), unlist(historical_child_DOB_PERSONS), unlist(hist_child_preg_DOB)))
colnames(historical_neonates)<-c("person_id", "DOB_persons", "DOB")
fwrite(historical_neonates, paste0(historical_neonate_folder,"historical_neonates.csv"))


##################################################################################
# copy over CDM files for neonates


historical_neonate_id<-historical_neonates$person_id

actual_tables_CDM<-list()
    actual_tables_CDM$EVENTS<-list.files(paste0(path_CDM,"/"), pattern="^EVENTS")
    actual_tables_CDM$MEDICAL_OBSERVATIONS<-list.files(paste0(path_CDM,"/"), pattern="^MEDICAL_OB")
    actual_tables_CDM$SURVEY_OBSERVATIONS<-list.files(paste0(path_CDM,"/"), pattern="^SURVEY_OB")
    actual_tables_CDM$MEDICINES<-list.files(paste0(path_CDM,"/"), pattern="^MEDICINES")
    actual_tables_CDM$VACCINES<-list.files(paste0(path_CDM,"/"), pattern="^VACCINES")
    actual_tables_CDM$SURVEY_ID<-list.files(paste0(path_CDM,"/"), pattern="^SURVEY_ID")
    actual_tables_CDM$EUROCAT<-list.files(paste0(path_CDM,"/"), pattern="^EUROCAT")
    actual_tables_CDM$PERSONS<-list.files(paste0(path_CDM,"/"), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(path_CDM,"/"), pattern = "\\.csv$")
table_list<-unlist(actual_tables_CDM)


for (i in 1:length(table_list)){
  my_table<-fread(paste0(path_CDM,table_list[i]))
  my_historical_table<- my_table[(my_table$person_id%in%historical_neonate_id),]
  
  fwrite(my_historical_table,paste0(historical_neonate_folder,table_list[i]))
}
