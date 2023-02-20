#+ phase-4, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####         Bootstap 2018 and 2019 Condition 0 SGPs for Illinois          ####
####                                                                       ####
###############################################################################

#' # Phase 4 - Threshold for Comparison Metrics
#'
#' This section presents and explains the code used to conduct the comparison
#' metric bootstaping method for constructing confidence intervals.
#'
#' ## Load SGP package and Illinois data
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

SGPstateData[["IL"]][["Achievement"]][["Knots_Boundaries"]] <-
    list(
      ELA =
        SGPstateData[["PARCC"]][["Achievement"]][["Knots_Boundaries"]][["ELA_SS"]],
      MATHEMATICS =
        SGPstateData[["PARCC"]][["Achievement"]][["Knots_Boundaries"]][["MATHEMATICS_SS"]]
    )
SGPstateData[["IL"]][["Achievement"]][["Cutscores"]] <-
    list(
      ELA =
        SGPstateData[["PARCC"]][["Achievement"]][["Cutscores"]][["ELA_SS"]],
      MATHEMATICS =
        SGPstateData[["PARCC"]][["Achievement"]][["Cutscores"]][["MATHEMATICS_SS"]]
    )


#  Load cleaned, merged and formatted data and pre-calculated Condition 0 matrices
Illinois_Data_LONG <-
    freadZIP(
    "Data/Phase_2-Student_Growth/Student_LongTestData_Illinois_2016-2019_AVI.csv.zip"
    )[YEAR < 2020][,
        GRADE := as.character(GRADE)
    ][,
        c("SEM", "SCALE_SCORE_Short", "ACHIEVEMENT_LEVEL_Short") := NULL
    ][,
        PROFICIENCY := fcase(
            ACHIEVEMENT_LEVEL %in% c("Level 1", "Level 2", "Level 3"), 0L,
            ACHIEVEMENT_LEVEL %in% c("Level 4", "Level 5"), 1L
        )
    ]

# load("./Condition_0/Condition_0_CoefMatrices.rda")

#  Load state-provided accountability data
state_acct_data_18 <-
  fread("Data/State_Files/School_AcctData_IL_EM_2018_LK.csv")

setnames(
  state_acct_data_18,
  c("School_ID", "ELP_ProfRate"),
  c("SchoolID", "ProgELP")
)

# How should NA in indicators be dealt with?
# Impute where missing
sqss.vars <- "ChronicAbsent"
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
  fread("Data/State_Files/School_AcctData_IL_EM_2019_LK.csv")

setnames(
  state_acct_data_19,
  c("School_ID", "ELP_ProfRate"),
  c("SchoolID", "ProgELP")
)

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
  SQSS := rowMeans(.SD, na.rm = TRUE),  #  Keep this option per LK (2/1/23)
  .SDcols = (sqss.vars)
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


# bootstrapCond0(
#     sgp_data = Illinois_Data_LONG,
#     config = config.c0.2018,
#     state.abbr = "IL",
#     state.name = "Illinois",
#     fyear = "2018",
#     workers = boot.workers,
#     bootstrap.n = 100,
#     # coef_matrices = CoefMatrices,
#     state_indicators = state_acct_data_18
# )

# bootstrapCond0(
#     sgp_data = Illinois_Data_LONG,
#     config = config.c0.2019,
#     state.abbr = "IL",
#     state.name = "Illinois",
#     fyear = "2019",
#     workers = boot.workers,
#     bootstrap.n = 100,
#     # coef_matrices = CoefMatrices,
#     state_indicators = state_acct_data_19
# )
