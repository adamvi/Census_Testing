#+ phase-4, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####         Bootstrap 2018 and 2019 Condition 0 SGPs for Georgia          ####
####                                                                       ####
###############################################################################

#' # Phase 4 - Threshold for Comparison Metrics
#'
#' This section presents and explains the code used to conduct the comparison
#' metric bootstraping method for constructing confidence intervals.
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
        "Data/Phase_1-Cleaned_Data/Student_LongTestData_Georgia_2016-2022_AVI.csv.zip"
    )[YEAR < 2020][,
        c("SEM", "SCALE_SCORE_Short", "ACHIEVEMENT_LEVEL_Short") := NULL
    ][,
      PROFICIENCY := fcase(
        ACHIEVEMENT_LEVEL %in% c("Beginning Learner", "Developing Learner"), 0L,
        ACHIEVEMENT_LEVEL %in% c("Proficient Learner", "Distinguished Learner"), 1L
      )
    ]

# load("./Condition_0/Condition_0_CoefMatrices.rda")

#  Load state-provided accountability data
state_acct_data_18 <-
  fread("Data/Phase_1-Cleaned_Data/School_AcctData_Georgia_2018_AVI.csv")
state_acct_data_19 <-
  fread("Data/Phase_1-Cleaned_Data/School_AcctData_Georgia_2019_AVI.csv")


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

boot.workers <- list(TAUS = 15)
# boot.workers <- list(PERCENTILES = 15)
set.seed(4224)


bootstrapCond0(
    sgp_data = Georgia_Data_LONG,
    config = config.c0.2018,
    state.abbr = "GA",
    state.name = "Georgia",
    fyear = "2018",
    workers = boot.workers,
    bootstrap.n = 100,
    # coef_matrices = CoefMatrices,
    state_indicators = state_acct_data_18
)

bootstrapCond0(
    sgp_data = Georgia_Data_LONG,
    config = config.c0.2019,
    state.abbr = "GA",
    state.name = "Georgia",
    fyear = "2019",
    workers = boot.workers,
    bootstrap.n = 100,
    state_indicators = state_acct_data_19
)
