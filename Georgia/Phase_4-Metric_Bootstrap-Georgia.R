#+ phase-4, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####          Bootstap 2018 and 2019 Condition 0 SGPs for Georgia          ####
####                                                                       ####
###############################################################################

#' # Phase 4 - Threshold for Comparison Metrics
#'
#' This section presents and explains the code used to conduct the comparison
#' metric bootstaping method for constructing confidence intervals.
#'
#' ## Load SGP package and Georgia data
#'
#' The `SGP` package is required for all growth percentile analyses.
#'
#+ sgp-calc-pkg, echo = TRUE, purl = TRUE
require(SGP)
require(data.table)
source("../functions/freadZIP.R")
source("../functions/bootstrapCond0.R")
source("../functions/schoolAggrGator.R")
source("../functions/accountabilityModel.R")

#  Load cleaned, merged and formatted data
Georgia_Data_LONG <-
    freadZIP(
        "Data/Cleaned_Data/Student_LongTestData_Georgia_2016-2022_AVI.csv.zip"
    )[YEAR < 2020][,
        c("SEM", "SCALE_SCORE_Short", "ACHIEVEMENT_LEVEL_Short") := NULL
    ][,
      PROFICIENCY := fcase(
        ACHIEVEMENT_LEVEL %in% c("Beginning Learner", "Developing Learner"), 0L,
        ACHIEVEMENT_LEVEL %in% c("Proficient Learner", "Distinguished Learner"), 1L
      )
    ]

load("./Condition_0/Condition_0_CoefMatrices.rda")

#  Load state-provided accountability data
state_acct_data_18 <-
  fread("Data/Cleaned_Data/School_AcctData_Georgia_2018_EK.csv")

state_acct_data_18[,
  ELA_PartRate := as.numeric(ELA_PartRate)
][,
  Math_PartRate := as.numeric(Math_PartRate)
][,
  ProgELP := as.numeric(ProgELP)
][,
  Beyond_The_Core  := as.numeric(Beyond_The_Core)
][,
  Literacy := as.numeric(Literacy)
][,
  Student_Attendance := as.numeric(Student_Attendance)
]

# Impute indicators where missing
sqss.vars <- c("Literacy", "Beyond_The_Core", "Student_Attendance")
for (sqss in sqss.vars) {
  tmp.data <-
    state_acct_data_18[!(is.na(ELA_PartRate) | is.na(Math_PartRate))]
  tmp.rq <-
    quantreg::rq(
      formula = as.formula(paste(sqss, "~ Group")),
      data = tmp.data
    )
  tmp.pred <- predict(object = tmp.rq, newdata = tmp.data)
  tmp.pred[tmp.pred < 0 & !is.na(tmp.pred)] <- 0
  state_acct_data_18[
    !(is.na(ELA_PartRate) | is.na(Math_PartRate)),
    TMP_PRED := round(tmp.pred, 3)
  ][
    is.na(get(sqss)),
    eval(sqss) := TMP_PRED
  ][,
    TMP_PRED := NULL
  ]
}

state_acct_data_18[,
  SQSS := rowMeans(.SD, na.rm = TRUE),
  .SDcols = (sqss.vars)
]

state_acct_data_19 <-
  fread("Data/Cleaned_Data/School_AcctData_Georgia_2019_EK.csv")

state_acct_data_19[,
  ELA_PartRate := as.numeric(ELA_PartRate)
][,
  Math_PartRate := as.numeric(Math_PartRate)
][,
  ProgELP := as.numeric(ProgELP)
][,
  Beyond_The_Core  := as.numeric(Beyond_The_Core)
][,
  Literacy := as.numeric(Literacy)
][,
  Student_Attendance := as.numeric(Student_Attendance)
]

# Impute 2019 where missing
for (sqss in sqss.vars) {
  tmp.data <-
    state_acct_data_19[!(is.na(ELA_PartRate) | is.na(Math_PartRate))]
  tmp.rq <-
    quantreg::rq(
      formula = as.formula(paste(sqss, "~ Group")),
      data = tmp.data
    )
  tmp.pred <- predict(object = tmp.rq, newdata = tmp.data)
  tmp.pred[tmp.pred < 0 & !is.na(tmp.pred)] <- 0
  state_acct_data_19[
    !(is.na(ELA_PartRate) | is.na(Math_PartRate)),
    TMP_PRED := round(tmp.pred, 3)
  ][
    is.na(get(sqss)),
    eval(sqss) := TMP_PRED
  ][,
    TMP_PRED := NULL
  ]
}

state_acct_data_19[,
  SQSS := rowMeans(.SD, na.rm = TRUE),
  .SDcols = c("Beyond_The_Core", "Literacy", "Student_Attendance")
]

# ProgELP only in "EL" rows in 2019.  Who should this count for? All/EL only? Every Group?
ProgELP_Lookup <-
  state_acct_data_19[
    Group == "EL" & !is.na(ProgELP),
      .(SchoolID, Group, ProgELP)
  ][,
    Group := "All"
  ]

setkey(ProgELP_Lookup, SchoolID, Group)
setkey(state_acct_data_19, SchoolID, Group)
state_acct_data_19 <- ProgELP_Lookup[state_acct_data_19]
state_acct_data_19[
  !is.na(i.ProgELP), ProgELP := i.ProgELP
][,
  i.ProgELP := NULL
]


#+ phase-4-config, echo = TRUE, purl = TRUE
source("SGP_CONFIG/Condition_0.R")

GRADE_3.2018.config <-
    list(
        ELA.2018 = list(
            sgp.content.areas = "ELA",
            sgp.panel.years = "2018",
            sgp.grade.sequences = list("3")
        ),
        MATHEMATICS.2018 = list(
            sgp.content.areas = "MATHEMATICS",
            sgp.panel.years = "2018",
            sgp.grade.sequences = list("3")
        )
    )

GRADE_3.2019.config <-
    list(
        ELA.2019 = list(
            sgp.content.areas = "ELA",
            sgp.panel.years = "2019",
            sgp.grade.sequences = list("3")
        ),
        MATHEMATICS.2019 = list(
            sgp.content.areas = "MATHEMATICS",
            sgp.panel.years = "2019",
            sgp.grade.sequences = list("3")
        )
    )

config.c0.2018 <-
    c(GRADE_3.2018.config,
      ELA_2018.config,
      MATHEMATICS_2018.config
    )
config.c0.2019 <-
    c(GRADE_3.2019.config,
      ELA_2019.config,
      MATHEMATICS_2019.config
    )

boot.workers <- list(PERCENTILES = 10)
set.seed(4224)


bootstrapCond0(
    sgp_data = Georgia_Data_LONG,
    config = config.c0.2018,
    state.abbr = "GA",
    state.name = "Georgia",
    fyear = "2018",
    workers = boot.workers,
    bootstrap.n = 100,
    coef_matrices = CoefMatrices,
    state_indicators = state_acct_data_18
)

system.time(
bootstrapCond0(
    sgp_data = Georgia_Data_LONG,
    config = config.c0.2019,
    state.abbr = "GA",
    state.name = "Georgia",
    fyear = "2019",
    workers = boot.workers,
    bootstrap.n = 100,
    coef_matrices = CoefMatrices,
    state_indicators = state_acct_data_19
)
)
