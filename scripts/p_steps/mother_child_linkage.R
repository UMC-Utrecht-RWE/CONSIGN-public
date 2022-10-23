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

RELATIONSHIP_TABLE<- fread(path_PR,PR_name)

PERSONS<-fread(paste0(path_CDM,"PERSONS.csv"), select = c("person_id", "day_of_birth", "month_of_birth", "year_of_birth"))

CHILDREN<-PERSONS[PERSONS$year_of_birth>2017,]

CHILDREN$comp_day_birth<-CHILDREN$day_of_birth
CHILDREN$comp_day_birth[is.na(CHILDREN$day_of_birth)]<-15

DOB<-paste0(CHILDREN$comp_day_birth, "/", CHILDREN$month_of_birth, "/",CHILDREN$year_of_birth)
CHILDREN$DOB<-as.Date(DOB, format="%d/%m/%Y")
CHILDREN$DOB_numeric<-as.numeric(CHILDREN$DOB)

cases<- fread(paste0(cov_pos_pan_preg_folder,"cov_pos_preg.csv"))
controls<-fread(paste0(matched_folder, "matches_pregnant_cov_neg.csv"))

controls$type_of_pregnancy_end<-sample(c("LB", "LB", "SA"), replace = T,size = nrow(controls))
cases$type_of_pregnancy_end<-sample(c("LB", "LB", "SA"), replace = T, size=nrow(cases))


cases_mom_id<-cases$person_id [cases$type_of_pregnancy_end=="LB"]
cases_pregnancy_id<-cases$pregnancy_id [cases$type_of_pregnancy_end=="LB"]
cases_DOB<-cases$pregnancy_end_date[cases$type_of_pregnancy_end=="LB"]
case_child<-list()
case_child_DOB_PERSONS<-list()


controls_mom_id<-controls$person_id[controls$type_of_pregnancy_end=="LB"]
controls_DOB<-controls$pregnancy_end_date[controls$type_of_pregnancy_end=="LB"]
control_child<-list()
control_child_DOB_PERSONS<-list()

# simulate PR table >_<
# 
CHILDREN$related_id<- sample(PERSONS$person_id[PERSONS$year_of_birth<1995], nrow(CHILDREN))
CHILDREN$meaning_of_relationship<-sample(c("birth_mother", "father"), replace=T,nrow(CHILDREN))
all_mom_ids<-c(cases_mom_id, controls_mom_id)
CHILDREN$related_id[1:length(all_mom_ids)]<-all_mom_ids
all_DOB<-c(cases_DOB, controls_DOB)
sample_error<-sample(c(-5:5), replace=T, length(all_DOB))
CHILDREN$DOB_numeric[1:length(all_DOB)]<-(all_DOB)-sample_error
CHILDREN$meaning_of_relationship[1:length(all_DOB)]<-"birth_mother"
#
#
PERSONS_RELATIONS<-CHILDREN %>% select("person_id", "related_id", "meaning_of_relationship")
# 
# ################################################################################
# 

cases_PR<-PERSONS_RELATIONS[PERSONS_RELATIONS$related_id%in%cases_mom_id,]
# all children of case mother == child_id == cases_PR$person_id
  # check child_ids against DOB
case_all_children<-CHILDREN[CHILDREN$person_id%in%cases_PR$person_id]

# multiple children per woman-- need to group by mom_id
# cases_PR wide--> long (melt)
my_id_vars<-colnames(cases_PR)[(colnames(cases_PR)%exclude%"related_id")]
long_cases_PR<-melt(cases_PR,id.vars = my_id_vars)
case_all_children$mom_id<-long_cases_PR$value
 
# for each mom_id and case_DOB combination, test DOB of CHILDREN with mom_id

# POSSIBLE that 1 mom has 2 case pregnancies 
# that's fine- different DOB
# BUT what about TWINS? 

for(i in 1:length(cases_mom_id)){
 mom<- cases_mom_id[i]
 case_DOB<-cases_DOB[i]
  offspring<-case_all_children[case_all_children$mom_id==mom,]
  offspring$days_to_DOB<- (offspring$DOB_numeric)-(case_DOB)
  case_child[[i]]<-offspring$person_id[abs(offspring$days_to_DOB)<31]
  case_child_DOB_PERSONS[[i]]<-offspring$DOB_numeric[abs(offspring$days_to_DOB)<31]
}

case_target_children<-as.data.frame(cbind(unlist(case_child), unlist(case_child_DOB_PERSONS)))
colnames(case_target_children)<-c("person_id", "date_of_birth_PERSONS")
fwrite(case_target_children, paste0(case_neonate_folder,"cases_neonates.csv"))

###################################################################

controls_PR<-PERSONS_RELATIONS[PERSONS_RELATIONS$related_id%in%controls_mom_id,]
# all children of control mother == child_id == controls_PR$person_id
# check child_ids against DOB
control_all_children<-CHILDREN[CHILDREN$person_id%in%controls_PR$person_id]

# multiple children per woman-- need to group by mom_id
# controls_PR wide--> long (melt)
my_id_vars<-colnames(controls_PR)[(colnames(controls_PR)%exclude%"related_id")]
long_controls_PR<-melt(controls_PR,id.vars = my_id_vars)
control_all_children$mom_id<-long_controls_PR$value

# for each mom_id and control_DOB combination, test DOB of CHILDREN with mom_id

for(i in 1:length(controls_mom_id)){
  mom<- controls_mom_id[i]
  control_DOB<-controls_DOB[i]
  offspring<-control_all_children[control_all_children$mom_id==mom,]
  offspring$days_to_DOB<- (offspring$DOB_numeric)-(control_DOB)
  control_child[[i]]<-offspring$person_id[abs(offspring$days_to_DOB)<30]
  control_child_DOB_PERSONS[[i]]<-offspring$DOB_numeric[abs(offspring$days_to_DOB)<30]
}

control_target_children<-as.data.frame(cbind(unlist(control_child), unlist(control_child_DOB_PERSONS)))
colnames(control_target_children)<-c("person_id", "date_of_birth_PERSONS")

fwrite(control_target_children, paste0(control_neonate_folder,"control_neonates.csv"))

##################################################################################
# copy over CDM files for neonates

case_neonate_id<-case_target_children$person_id
control_neonate_id<-control_target_children$person_id


CDM_source<-fread(paste0(path_CDM,"CDM_SOURCE.csv"))
DAP<-CDM_source$data_access_provider_name

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
  my_case_table<- my_table[(my_table$person_id%in%case_neonate_id),]
  
  fwrite(my_case_table,paste0(case_neonate_folder,table_list[i]))
}

for (i in 1:length(table_list)){
  my_table<-fread(paste0(path_CDM,table_list[i]))
  my_control_table<- my_table[(my_table$person_id%in%control_neonate_id),]
  
  fwrite(my_control_table,paste0(control_neonate_folder,table_list[i]))
}

