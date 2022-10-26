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

if(DAP!="Bordeaux"){events_tables<-IMPORT_PATTERN(pat="EVENTS_SLIM", dir = preselect_folder)}else{ events_tables<-IMPORT_PATTERN(pat="EVENTS",dir=preselect_folder)}

# something going wrong with codes with leading 0s

full_codelist<-IMPORT_PATTERN(pat="codelist_CONSIGN", dir=projectFolder)

# make sure everything is upper case
# full_codelist$event_definition<-toupper(full_codelist$event_definition)
# full_codelist$code<-toupper(full_codelist$code)
# full_codelist$coding_system<-toupper(full_codelist$coding_system)

covid_codelist<-full_codelist[full_codelist$event_definition=="COVID19 diagnosis",]


CreateConceptDatasets(codesheet = covid_codelist, file = events_tables, c.voc="coding_system", 
                      c.concept="event_definition", c.codes="code", c.startwith = "ICD9CM",
                      f.code="event_code", f.voc="event_record_vocabulary", path = preselect_folder,
                      method = "loop", group = T, f.name = NULL, db = NULL )



