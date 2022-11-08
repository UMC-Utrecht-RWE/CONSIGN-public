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
if(DAP!="Bordeaux"){buffer<-30} else{buffer<-(366/2)}

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

case<- fread(paste0(matched_folder,"matches_cases.csv"))
controls<-fread(paste0(matched_folder, "matches_pregnant_cov_neg.csv"))

case_mom_id<-case$person_id [case$type_of_pregnancy_end=="LB"]
case_DOB<-case$pregnancy_end_date[case$type_of_pregnancy_end=="LB"]
case_child<-list()
case_child_DOB_PERSONS<-list()
case_child_preg_DOB<-list()


controls_mom_id<-controls$person_id[controls$type_of_pregnancy_end=="LB"]
controls_DOB<-controls$pregnancy_end_date[controls$type_of_pregnancy_end=="LB"]
control_child<-list()
control_child_DOB_PERSONS<-list()
conrol_child_preg_DOB<-list()


# in each matched cohort (cases, P_control) there's only one pregnancy per woman
# but a pregnancy may have 2 or more love born babies 

case_PR<-PERSONS_RELATIONS[PERSONS_RELATIONS$related_id%in%case_mom_id,]
# all children of case mother == child_id == case_PR$person_id
  # check child_ids against DOB
case_all_children<-CHILDREN[CHILDREN$person_id%in%case_PR$person_id]

case_all_children<-merge(case_PR, case_all_children, by = 'person_id')

# my_DOB is end_date_pregnancy from matched cohort pregnancy data

case_child_mom_id<-list()

for(i in 1:length(case_mom_id)){
  mom<- case_mom_id[i]
  my_DOB<-case_DOB[i]
  offspring<-case_all_children[case_all_children$related_id==mom,]
  offspring$days_to_DOB<- (offspring$DOB_numeric)-(my_DOB)
  print(offspring$days_to_DOB)
  case_child[[i]]<-offspring$person_id[abs(offspring$days_to_DOB)<buffer]
  case_child_DOB_PERSONS[[i]]<-offspring$DOB_numeric[abs(offspring$days_to_DOB)<buffer]
  case_child_preg_DOB[[i]]<-rep(my_DOB, length(case_child[[i]]))
  case_child_mom_id[[i]]<-mom
}

case_neonates<-as.data.frame(cbind(unlist(case_child), unlist(case_child_DOB_PERSONS), unlist(case_child_preg_DOB), unlist(case_child_mom_id)))
colnames(case_neonates)<-c("person_id", "DOB_persons", "DOB", "mom_id")

fwrite(case_neonates, paste0(case_neonate_folder,"case_neonates.csv"))

###################################################################

controls_PR<-PERSONS_RELATIONS[PERSONS_RELATIONS$related_id%in%controls_mom_id,]
# all children of control mother == child_id == controls_PR$person_id
# check child_ids against DOB
control_all_children<-CHILDREN[CHILDREN$person_id%in%controls_PR$person_id]

control_all_children<-merge(controls_PR,control_all_children, by="person_id")

# for each mom_id and control_DOB combination, test DOB of CHILDREN with mom_id

control_child_mom_id<-list()

for(i in 1:length(controls_mom_id)){
  mom<- controls_mom_id[i]
  my_DOB<-controls_DOB[i]
  offspring<-control_all_children[control_all_children$related_id==mom,]
  offspring$days_to_DOB<- (offspring$DOB_numeric)-(my_DOB)
  print(offspring$days_to_DOB)
  control_child[[i]]<-offspring$person_id[abs(offspring$days_to_DOB)<buffer]
  control_child_DOB_PERSONS[[i]]<-offspring$DOB_numeric[abs(offspring$days_to_DOB)<buffer]
  conrol_child_preg_DOB[[i]]<-rep(my_DOB, length(control_child[[i]]))
  control_child_mom_id[[i]]<-mom
}

control_neonates<-as.data.frame(cbind(unlist(control_child), unlist(control_child_DOB_PERSONS), unlist(conrol_child_preg_DOB), unlist(control_child_mom_id)))
colnames(control_neonates)<-c("person_id", "DOB_persons", "DOB", "mom_id")

fwrite(control_neonates, paste0(control_neonate_folder,"control_neonates.csv"))

