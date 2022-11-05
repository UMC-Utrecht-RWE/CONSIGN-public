#' @param codesheet data.table file with the codes, concept and coding system
#' @param file CDM table with codes
#' @param c.voc codesheet coding system column name
#' @param c.concept codesheet concept column name
#' @param c.codes codesheet code column name
#' @param coding systems that merges based on start with
#' @param f.code file code column name
#' @param f.voc file coding system column name
#' @param path location to write the RDS files to per concept name
#' @param method if SQL, a join is used. If loop a loop is used to subset
#' 



events_tables_list<-list.files(paste0(preselect_folder), pattern = "EVENTS")

# something going wrong with codes with leading 0s

full_codelist<-IMPORT_PATTERN(pat="codelist_CONSIGN", dir=projectFolder)

# make sure everything is upper case
# full_codelist$event_definition<-toupper(full_codelist$event_definition)
# full_codelist$code<-toupper(full_codelist$code)
# full_codelist$coding_system<-toupper(full_codelist$coding_system)

covid_codelist<-full_codelist[full_codelist$event_definition=="COVID19 diagnosis",]

for(i in 1:length(events_tables_list)){
  events_tables<-fread(paste0(preselect_folder, events_tables_list[i]))

  CreateConceptDatasets(codesheet = covid_codelist, file = events_tables, c.voc="coding_system", 
                      c.concept="event_definition", c.codes="code", c.startwith = c("ICD9CM", "ICD9",  "ICD10", "ICD10CM", "ICD10DA"),
                      f.code="event_code", f.voc="event_record_vocabulary", path = preselect_folder,
                      method = "loop", group = T, f.name = NULL, db = NULL )
  cov_ev_data<-readRDS(paste0(preselect_folder,"COVID19 diagnosis.rds"))
  fwrite(cov_ev_data, paste0(preselect_folder, "covid19_diagnosis_",i,".csv"))
  }



