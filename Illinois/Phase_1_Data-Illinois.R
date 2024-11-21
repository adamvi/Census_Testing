#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                  Data Cleaning and Prep -- Illinois                   ####
####                                                                       ####
###############################################################################
if (!dir.exists("./Data/Phase_1-Cleaned_Data"))
     dir.create("./Data/Phase_1-Cleaned_Data", recursive = TRUE)

#' #  Data merging, cleaning and preparation
#'
#' For this simulation analysis we will be using data provided by the Illinois
#' Department of Education (GaDOE) for this project...
#'
#' This section of the appendix assumes the user is operating with their
#' working directory set to the state level directory (e.g., "*./Illinois/*".

#+ data-prep-wd, echo = TRUE, purl = TRUE, eval = FALSE
# setwd("./Illinois")

#' ## Load packages and custom functions.
#'
#' The following `R` packages are required for the data source, cleaning and
#' augmentation.
#'
#+ data-prep-pkg, echo = TRUE, purl = TRUE
require(SGP)
require(data.table)

#' ## General data setup and cleaning
#'
#' Things we need to do...
#'
#+ data-prep-getdata, echo = TRUE, purl = TRUE, eval = FALSE
# Load IL data from Learning Loss Analyses conducted by the Center
load("~/Sync/Center/SGP/State_Alt_Analyses/Illinois/Learning_Loss_Analysis/Data/Report_Data.Rdata")
Illinois_Data_LONG <-
    Report_Data[["State_Assessment"]][
        YEAR < 2020
    ][,
        (c(1:5, 7:8, 14, 16, 21:23)),
        with = FALSE
    ]

#+ data-prep-rename, echo = TRUE, purl = TRUE, eval = FALSE
gw_var_names <-
    c("SCALE_SCORE", "SchoolID", "Race", "SWD", "EconDis", "EL")

setnames(
  Illinois_Data_LONG,
  old = c("SCALE_SCORE_ACTUAL", "SCHOOL_NUMBER",
          "FederalRaceEthnicity", "StudentWithDisabilities",
          "EconomicDisadvantageStatus", "EnglishLearnerEL"
        ),
  new = gw_var_names
)


#' ### Tiddy up `SGP` required variables and student demographics
#'
#+ data-prep-tidy, echo = TRUE, purl = TRUE, eval = FALSE

##    Demographics
Illinois_Data_LONG[,
    Race :=
        factor(Race,
               levels = 1:8,
               labels = c("Native", "Asian", "Black", "Hispanic",
                          "HWPI", "White", "Multi", "Other")
        )
]
Illinois_Data_LONG[, Race := as.character(Race)]
# 1 = "Native"
# 2 = "Asian"
# 3 = "Black"
# 4 = "Hispanic"
# 5 = "HWPI"
# 6 = "White"
# 7 = "Multi"
# 8 = ???

Illinois_Data_LONG[,
    EconDis := ifelse(EconDis == 0, "EconDis: No", "EconDis: Yes")
]
Illinois_Data_LONG[, EL := ifelse(EL == 0, "EL: No", "EL: Yes")]
Illinois_Data_LONG[, SWD := ifelse(SWD == 0, "SWD: No", "SWD: Yes")]

#' ### Create `VALID_CASE` and invalidate duplicates
#+ data-prep-vc, echo = TRUE, purl = TRUE, eval = FALSE
Illinois_Data_LONG[, VALID_CASE := "VALID_CASE"]
setkey(Illinois_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID, SCALE_SCORE)
setkey(Illinois_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID)
Illinois_Data_LONG |> duplicated(by = key(Illinois_Data_LONG)) |> sum()
# 0 duplicates - (((take the highest score if any exist)))
# dupl <- duplicated(Illinois_Data_LONG, by = key(Illinois_Data_LONG))
Illinois_Data_LONG[
    which(duplicated(Illinois_Data_LONG, by = key(Illinois_Data_LONG)))-1,
      VALID_CASE := "INVALID_CASE"
]
table(Illinois_Data_LONG[, .(YEAR, VALID_CASE), GRADE])
# Illinois_Data_LONG <- Illinois_Data_LONG[VALID_CASE == "VALID_CASE"]


#' ## Create `SCALE_SCORE_Short` - simulated half test score
#'
#' Condition 4 requires a score variable that has been perturbed with additional
#' error/noise to simulate the impact of using half-length tests.
#' The code below constructs a table of the reliability estimates obtained from
#' the Illinois State Board of Education's website.

#+ data-prep-test-rel, echo = TRUE, purl = TRUE, eval = FALSE
test_reliability <-
    data.table::fread(
        text =
           "CONTENT_AREA,  GRADE,  reliability
            ELA,             3,       0.86
            MATHEMATICS,     3,       0.91
            ELA,             4,       0.87
            MATHEMATICS,     4,       0.91
            ELA,             5,       0.86
            MATHEMATICS,     5,       0.90
            ELA,             6,       0.88
            MATHEMATICS,     6,       0.89
            ELA,             7,       0.89
            MATHEMATICS,     7,       0.88
            ELA,             8,       0.90
            MATHEMATICS,     8,       0.84",
            # ELA,           9,       0.89,
            # ELA,          10,       0.89,
            # ELA,          11,       0.86,
            # ALGEBRA_I,   EOCT,      0.84,
            # GEOMETRY,    EOCT,      0.84,
            # ALGEBRA_II,  EOCT,      0.85",
        header = TRUE
    )[, GRADE := as.character(GRADE)] |>
        setkeyv(c("CONTENT_AREA", "GRADE"))

#' From these reliability values, a modified estimate of the half-test
#' reliability is calculated.  Along with these revised reliabilities, the
#' standard deviation of each test is obtained from the observed data and
#' used to calculate the standard error of measurement (SEM).

test_std_dev <-
    Illinois_Data_LONG[
        YEAR %in% 2016:2019,
        .(SD = sd(SCALE_SCORE, na.rm = TRUE)),
        keyby = c("CONTENT_AREA", "GRADE")
    ]

sem_lookup <- test_reliability[test_std_dev]
sem_lookup[,
    half_test_rel := (0.5 * reliability)/(1 - (0.5 * reliability))
][,
    SEM := SD * (sqrt(1 - reliability))
][,
    HT_SEM := SD * (sqrt(1 - half_test_rel))
][,
    c("reliability", "half_test_rel", "SD") := NULL
]

setkey(sem_lookup, CONTENT_AREA, GRADE)
setkey(Illinois_Data_LONG, CONTENT_AREA, GRADE)

Illinois_Data_LONG <- sem_lookup[Illinois_Data_LONG]

Illinois_Data_LONG[!is.na(SCALE_SCORE),
    SCALE_SCORE_Short :=
        rnorm(
            n = .N,
            mean = SCALE_SCORE,
            sd = SEM
        ) |> round(0)
][!is.na(SCALE_SCORE),
    SCALE_SCORE_Short_v2 :=
        rnorm(
            n = .N,
            mean = SCALE_SCORE,
            sd = HT_SEM
        ) |> round(0)
][!is.na(SCALE_SCORE),
    SCALE_SCORE_v2 :=
        rnorm(
            n = .N,
            mean = SCALE_SCORE,
            sd = SEM
        ) |> round(0)
]

##  Force scores outside LOSS/HOSS back into range
loss_hoss <-
    list(
      ELA =
        SGPstateData[["IL"]][["Achievement"]][["Knots_Boundaries"]][["ELA_SS"]],
      MATHEMATICS =
        SGPstateData[["IL"]][["Achievement"]][["Knots_Boundaries"]][["MATHEMATICS_SS"]]
    )
for (CA in c("ELA", "MATHEMATICS")) {
    for (G in 3:8) {
        tmp.loss <- loss_hoss[[CA]][[paste0("loss.hoss_", G)]][1]
        tmp.hoss <- loss_hoss[[CA]][[paste0("loss.hoss_", G)]][2]
        Illinois_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short < tmp.loss,
            SCALE_SCORE_Short := tmp.loss
        ]
        Illinois_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short > tmp.hoss,
            SCALE_SCORE_Short := tmp.hoss
        ]
        Illinois_Data_LONG[
            CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short_v2 < tmp.loss,
            SCALE_SCORE_Short_v2 := tmp.loss
        ]
        Illinois_Data_LONG[
            CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short_v2 > tmp.hoss,
            SCALE_SCORE_Short_v2 := tmp.hoss
        ]
        Illinois_Data_LONG[
            CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_v2 < tmp.loss,
            SCALE_SCORE_v2 := tmp.loss
        ]
        Illinois_Data_LONG[
            CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_v2 > tmp.hoss,
            SCALE_SCORE_v2 := tmp.hoss
        ]
    }
}

#  Summary of ACTUAL scale scores
Illinois_Data_LONG[
    YEAR %in% 2016:2019,
    as.list(round(summary(SCALE_SCORE), 1)),
    keyby = c("CONTENT_AREA", "GRADE")
]

#  Vs. "retest" imaginationland
Illinois_Data_LONG[
    YEAR %in% 2016:2019,
    as.list(round(summary(SCALE_SCORE_v2), 1)),
    keyby = c("CONTENT_AREA", "GRADE")
]

#  Vs. "short"
Illinois_Data_LONG[
    YEAR %in% 2016:2019,
    as.list(round(summary(SCALE_SCORE_Short), 1)),
    keyby = c("CONTENT_AREA", "GRADE")
]

##    Create a "Short" Achievement Level for the "Short" Score
SGPstateData[["IL"]][["Achievement"]][["Knots_Boundaries"]] <- loss_hoss
SGPstateData[["IL"]][["Achievement"]][["Cutscores"]] <-
    list(
      ELA =
        SGPstateData[["IL"]][["Achievement"]][["Cutscores"]][["ELA_SS"]],
      MATHEMATICS =
        SGPstateData[["IL"]][["Achievement"]][["Cutscores"]][["MATHEMATICS_SS"]]
    )

Illinois_Data_LONG[, ACHIEVEMENT_LEVEL := NULL]
SGP:::getAchievementLevel(
    sgp_data = Illinois_Data_LONG,
    state = "IL",
    achievement.level.name = "ACHIEVEMENT_LEVEL",
    scale.score.name = "SCALE_SCORE"
)
SGP:::getAchievementLevel(
    sgp_data = Illinois_Data_LONG,
    state = "IL",
    achievement.level.name = "ACHIEVEMENT_LEVEL_v2",
    scale.score.name = "SCALE_SCORE_v2"
)
SGP:::getAchievementLevel(
    sgp_data = Illinois_Data_LONG,
    state = "IL",
    achievement.level.name = "ACHIEVEMENT_LEVEL_Short",
    scale.score.name = "SCALE_SCORE_Short"
)
SGP:::getAchievementLevel(
    sgp_data = Illinois_Data_LONG,
    state = "IL",
    achievement.level.name = "ACHIEVEMENT_LEVEL_Short_v2",
    scale.score.name = "SCALE_SCORE_Short_v2"
)

# Illinois_Data_LONG[
#     YEAR %in% 2016:2019,
#     as.list(round(summary(SCALE_SCORE_Short), 1)),
#     keyby = c("CONTENT_AREA", "GRADE", "ACHIEVEMENT_LEVEL_Short")
# ]

#' ##  Save data
#'
#+ data-prep-save, echo = TRUE, purl = TRUE, eval = FALSE
setcolorder(
    Illinois_Data_LONG,
    c("VALID_CASE", "SchoolID", "YEAR", "ID", "CONTENT_AREA",
      "Race", "EconDis", "SWD", "EL",
      "SCALE_SCORE", "ACHIEVEMENT_LEVEL",
      "SCALE_SCORE_v2", "ACHIEVEMENT_LEVEL_v2",
      "SCALE_SCORE_Short", "ACHIEVEMENT_LEVEL_Short",
      "SCALE_SCORE_Short_v2", "ACHIEVEMENT_LEVEL_Short_v2"
    )
)
Illinois_Data_LONG[, c("SEM", "HT_SEM") := NULL]

fname <- "Data/Phase_1-Cleaned_Data/Student_LongTestData_Illinois_2016-2019_AVI.csv"
fwrite(Illinois_Data_LONG, file = fname)
zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")

#' ## Summary and notes
#'
#' * Data used for this project was provided by Pearson to the Center for
#'   Assessment for SGP analyses and Learning Loss analyses prior to this
#'   study.
#'   - A subset of 2016-2019 ELA and Math data are retained.
#' * Variables required by the `SGP` packages were renamed and formatted as
#'   necessary.
#' * Student demographic variables were formatted to be consistent across all
#'   states in the study.
#' * A `VALID_CASE` variable was created and used to identify duplicate records
#'   and other problematic cases.
