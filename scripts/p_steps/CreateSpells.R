
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

#modified by Ema Alsina MSc. e.m.alsina-2@umcutrecht.nl

# this script generates the observation spells used in the study.
# observations with a gap of 7 days or less are concatenated,
# and in the case of multiple spells, the most recent is taken, and the others are discarded, resulting in one spell per person_ID

print('Import and append observation periods files')

SUBP<-F

FlowChartCreateSpells <- list()

OBSERVATION_PERIODS <- fread(paste0(preselect_folder, "OBSERVATION_PERIODS.csv"))


step0<-"original data"

step0_nrow<-nrow(OBSERVATION_PERIODS)

step0_unique<-length(unique(OBSERVATION_PERIODS$person_id))

step1<-'Set start and end date to date format and if end date is empty fill with end study date'

print('Set start and end date to date format and if end date is empty fill with end study date')
lapply(c("op_start_date","op_end_date"), function (x) OBSERVATION_PERIODS <- OBSERVATION_PERIODS[,eval(x) := as.IDate(as.character(get(x)),"%Y%m%d")])

OBSERVATION_PERIODS <- OBSERVATION_PERIODS[is.na(op_end_date), op_end_date := end_study_date]

step1_nrow<-nrow(OBSERVATION_PERIODS)

step1_unique<-length(unique(OBSERVATION_PERIODS$person_id))


  ### THIS IS FOR SINGLE POPULATIONS 
  print("Create spells and select latest for ALL")

  before_CreateSpells <- nrow(OBSERVATION_PERIODS)

  OBSERVATION_PERIODS1 <- CreateSpells(
    dataset=OBSERVATION_PERIODS,
    id="person_id" ,
    start_date = "op_start_date",
    end_date = "op_end_date",
    overlap = FALSE,
    only_overlaps = F,
    gap_allowed = 7
  )

  print("CreateSpells run OK")

  after_CreateSpells<-nrow(OBSERVATION_PERIODS1)

  print("select most recent Observation Period")

  OBSERVATION_PERIODS1<- OBSERVATION_PERIODS1[(duplicated(OBSERVATION_PERIODS1$person_id, fromLast = TRUE)==F),]

  select_most_recent<-nrow(OBSERVATION_PERIODS1)

  print("CLEANUP OBSERVATION_PERIODS1")
  OBSERVATION_PERIODS1 <- OBSERVATION_PERIODS1[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]
  setnames(OBSERVATION_PERIODS1, "entry_spell_category", "op_start_date")
  setnames(OBSERVATION_PERIODS1, "exit_spell_category", "op_end_date")
  OBSERVATION_PERIODS1[,op_start_date := as.IDate(op_start_date)]
  OBSERVATION_PERIODS1[,op_end_date := as.IDate(op_end_date)]
  # for OP ending after study ends, 
  OBSERVATION_PERIODS1$end_date<-OBSERVATION_PERIODS1$op_end_date
  OBSERVATION_PERIODS1[(OBSERVATION_PERIODS1$end_date>end_study_date),]<-end_study_date
  fwrite(OBSERVATION_PERIODS1, file = paste0(preselect_folder,"ALL_OBS_SPELLS.csv"))


  ######################################################################################################################
  print("store FlowChart data on attrition")
  CreateSpellsStep<-c("original number of OBSERVATION PERIODS", "original number of unique personID",
                      "number of OBSERVATION PERIODS after concatenating observations with gaps <= 7 days",
                      "number of OBSERVATION PERIODS after selecting the most recent observation (one spell per unique ID)")

  OBS_number<-c(before_CreateSpells,step0_unique, after_CreateSpells, select_most_recent)

  FlowChartCreateSpells<-as.data.frame(cbind(CreateSpellsStep, OBS_number))

  fwrite(FlowChartCreateSpells, file = paste0(output_dir,"FlowChartCreateSpells.csv"))

  if(exists("FlowChartOverlap")){
    fwrite(FlowChartOverlap, file = paste0(output_dir,"SUBPOP_FlowChartOverlap.csv"))
    rm(FlowChartOverlap)
  }

  rm(before_CreateSpells,after_CreateSpells, select_most_recent, OBSERVATION_PERIODS, OBSERVATION_PERIODS1, FlowChartCreateSpells)
  gc()




