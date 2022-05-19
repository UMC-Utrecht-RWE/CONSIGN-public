
#' Aim
#'
#'Create cohorts using a list of inclusion criteria
#'
#'
#' @section ??
#'
#' @author
#' Roel Elbers
#'
#' @docType package
#' @name Flowchart
#' @keywords ??
#' @import data.table

NULL


#' @param file A data.table data frame
#' @param expr_list A named list with expression
#' @param id column to count if not interested in rows only
#' @param type 'exclude' if only reducing the file or 'boolean' if only store results of expressions
#' @param strata vector of columns to keep in output and if count over. 

#' @return A data.table data frame with an age band (bands/string), ages that are within the band (INT/integer), order for sorting (integer/Order) and an ageband with leading 0 (string/band0)

#' @export

All_population <- All_population[is.na(FIRST_PFIZER) & is.na(FIRST_OTHER), group := "Control"] # Not vaccinated population
All_population <- All_population[ (!is.na(FIRST_OTHER) & is.na(SECOND_OTHER) & is.na(THIRD_OTHER) & is.na(SECOND_PFIZER) & is.na(THIRD_PFIZER)) 
                                 | (!is.na(FIRST_OTHER) & !is.na(SECOND_OTHER) & is.na(THIRD_OTHER) & is.na(THIRD_PFIZER))
                                 | (!is.na(FIRST_OTHER) & !is.na(SECOND_OTHER) & !is.na(THIRD_OTHER)), group := "Only Other Vaccine"] # All patients vaccinated with only other vaccines

All_population <- All_population[!is.na(FIRST_PFIZER) | !is.na(SECOND_PFIZER) | !is.na(THIRD_PFIZER), group := "Atleast1 Pfizer Vaccine"] # All patients with atleast 1 dose of Pfizer




file <- All_population
expr_list <- FirstExclusionPersons
id = "person_id"
type = 'exclude'
strata = 'group'


type = 'boolean'

keep <- c(id, strata)

TEMP <- copy(file)
#j=1

for (j in 1:length(expr_list)){
  TEMP <- TEMP[, eval(names(expr_list)[j]) :=  fifelse(eval(expr_list[[j]]), T, F)]
}  

TEMP2 <- TEMP[, c(keep, names(expr_list)), with = F ]  

TEMP2$ALL <- rowSums(TEMP2[, names(expr_list) , with = F]) == length(expr_list)

file2 <- copy(file)[TEMP2[["ALL"]],]

COUNT <- data.table::melt(TEMP2, id.vars = c(id, strata), measure.vars = names(expr_list))

COUNT <- COUNT[, .(rows = .N, Cases = sum(value)), by = c(strata, "variable")]
subjects = uniqueN(get(id)),
aatest <- colSums(TEMP2[, names(expr_list), with = F])

#keep2 <- c(keep, names(expr_list))


Flowchart <- function(file, expr_list, id = NULL, type = 'exclude', strata = NULL){    
  
  FlowChart <- list()
  
  file <- copy(file)
  
  for (j in 1:length(expr_list)){
    
    before <- nrow(file)
    if(!is.null(id)) before2 <- length(unique(file[[id]]))
    
    file <- file[eval(expr_list[[j]]),]
    
    after <- nrow(file)
    if(!is.null(id)) after2 <- length(unique(file[[id]]))
    
    FlowChart[[paste("Step_",j)]]$step <- names(expr_list)[j]
    FlowChart[[paste("Step_",j)]]$before_rows <- before
    FlowChart[[paste("Step_",j)]]$after_rows <- after
    
    if(!is.null(id)) FlowChart[[paste("Step_",j)]]$before_subjects <- before2
    if(!is.null(id)) FlowChart[[paste("Step_",j)]]$after_subjects <- after2
    
    rm(before,after,before2,after2)
    gc()
  } 
  
  
  FlowChart <- as.data.table(do.call(rbind,FlowChart))
  
  return(list(file = file, FlowChart = FlowChart))
  
  
}
