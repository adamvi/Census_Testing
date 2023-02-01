#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                   Data Cleaning and Prep -- Georgia                   ####
####                                                                       ####
###############################################################################
if (!dir.exists("./Data/Cleaned_Data"))
     dir.create("./Data/Cleaned_Data", recursive = TRUE)

#' #  Data merging, cleaning and preparation
#'
#' For this simulation analysis we will be using data provided by the Georgia
#' Department of Education (GaDOE) for this project...
#'
#' This section of the appendix assumes the user is operating with their
#' working directory set to the state level directory (e.g., "*./Georgia/*".

#+ data-prep-wd, echo = TRUE, purl = TRUE, eval = FALSE
# setwd("./Georgia")

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
# Load raw GA data
Georgia_Data_LONG <-
    fread(
        "Data/Student Files/fy2016-2022_gmas-eog-detail-with-lexile_pipe.txt",
        sep = "|"
    )
# Best to get this out of the way ASAP - 3.65 mill extra records
Georgia_Data_LONG <-
    Georgia_Data_LONG[
        ASSESSMENT_SUBJECT_CODE %in% c("E", "M") &
        STUDENT_GRADE_LEVEL %in% 3:8,
    ]


#+ data-prep-rename, echo = TRUE, purl = TRUE, eval = FALSE
gw_var_names <-
    c("YEAR", "ID", "CONTENT_AREA",
      "Race", "SWD", "EconDis", "EL",
      "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"
    )

setnames(
  Georgia_Data_LONG,
  old = c("SCHOOL_YEAR", "STUDENT_ID32", "ASSESSMENT_SUBJECT_CODE",
          "RACE_ETHNICITY", "SWD_FLAG", "ECON_DISADVANTAGE_FLAG", "EL_FLAG",
          "STUDENT_GRADE_LEVEL", "ASSESSMENT_SCALE_SCORE", "PERFORMANCE_LEVEL"
        ),
  new = gw_var_names
)
setcolorder(Georgia_Data_LONG, gw_var_names)


#' We have also decided to make schools unique by the grade levels that they
#' serve (i.e. elementary and middle)

#+ data-prep-schid, echo = TRUE, purl = TRUE
Georgia_Data_LONG[,
    SchoolID := as.character(SYSTEM_ID*10000 + SCHOOL_ID)
][which(SYSTEM_ID > 1000),
    SchoolID := as.character(SYSTEM_ID)
][GRADE %in% 3:5,
    SchoolID := paste0(SchoolID, "E")
][GRADE %in% 6:8,
    SchoolID := paste0(SchoolID, "M")
]


#' ### Tiddy up `SGP` required variables and student demographics
#'
#+ data-prep-tidy, echo = TRUE, purl = TRUE, eval = FALSE
Georgia_Data_LONG[, SCALE_SCORE := as.numeric(SCALE_SCORE)]

Georgia_Data_LONG[, ACHIEVEMENT_LEVEL := as.factor(ACHIEVEMENT_LEVEL)]
setattr(
    Georgia_Data_LONG$ACHIEVEMENT_LEVEL, "levels",
    c("Beginning Learner", "Developing Learner",
      "Distinguished Learner", "Proficient Learner"
    )
)
Georgia_Data_LONG[, ACHIEVEMENT_LEVEL := as.character(ACHIEVEMENT_LEVEL)]

##    CONTENT_AREA must match meta-data in `SGPstateData`
Georgia_Data_LONG[
    CONTENT_AREA == "E", CONTENT_AREA := "ELA"
][
    CONTENT_AREA == "M", CONTENT_AREA := "MATHEMATICS"
]

##    Remove extraneous variables
Georgia_Data_LONG[,
  c("ASSESSMENT_TYPE_CODE", "LEXILE_SCALE_SCORE", "SYSTEM_ID", "SCHOOL_ID") := NULL
]


##    Demographics
Georgia_Data_LONG[, Race := factor(Race)]
setattr(Georgia_Data_LONG$Race, "levels",
        c("Black", "Hispanic", "Native American",
          "Multiracial", "Asian", "Asian", "White")
        )
Georgia_Data_LONG[, Race := as.character(Race)]
Georgia_Data_LONG[,
    EconDis := ifelse(EconDis == "N", "EconDis: No", "EconDis: Yes")
]
Georgia_Data_LONG[, EL := ifelse(EL == "N", "EL: No", "EL: Yes")]
Georgia_Data_LONG[, SWD := ifelse(SWD == "N", "SWD: No", "SWD: Yes")]

#' ### Create `VALID_CASE` and invalidate duplicates
#+ data-prep-vc, echo = TRUE, purl = TRUE, eval = FALSE
Georgia_Data_LONG[, VALID_CASE := "VALID_CASE"]
setkey(Georgia_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID, SCALE_SCORE)
setkey(Georgia_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID)
Georgia_Data_LONG |> duplicated(by = key(Georgia_Data_LONG)) |> sum()
# 767 duplicates - (((take the highest score if any exist)))
# dupl <- duplicated(Georgia_Data_LONG, by = key(Georgia_Data_LONG))
Georgia_Data_LONG[
    which(duplicated(Georgia_Data_LONG, by=key(Georgia_Data_LONG)))-1,
      VALID_CASE := "INVALID_CASE"
]
table(Georgia_Data_LONG[, .(YEAR, VALID_CASE), GRADE])
Georgia_Data_LONG <- Georgia_Data_LONG[VALID_CASE == "VALID_CASE"]


#' ## Create `SCALE_SCORE_Short` - simulated half test score
#'
#' Condition 4 requires a score variable that has been perturbed with additional
#' error/noise to simulate the impact of using half-length tests.
#' The code below constructs a table of the reliability estimates obtained from
#' the Georgia Department of Education's
#' ["Validity and Reliability Brief"](https://www.gadoe.org/Curriculum-Instruction-and-Assessment/Assessment/Documents/Milestones/Technical_Documents/2020-21_GA_Milestones_Validity_Reliability_Brief.pdf)

#+ data-prep-test-rel, echo = TRUE, purl = TRUE, eval = FALSE
test_reliability <-
    data.table::fread(
        text =
           "CONTENT_AREA,  GRADE,  Reliability
            ELA,             3,       0.92
            MATHEMATICS,     3,       0.93
            ELA,             4,       0.92
            MATHEMATICS,     4,       0.94
            ELA,             5,       0.90
            MATHEMATICS,     5,       0.94
            ELA,             6,       0.91
            MATHEMATICS,     6,       0.93
            ELA,             7,       0.91
            MATHEMATICS,     7,       0.92
            ELA,             8,       0.91
            MATHEMATICS,     8,       0.93",
            # ELA,            EOCT,     0.91
            # MATHEMATICS,    EOCT,     0.90
        header = TRUE,
        key = c("CONTENT_AREA", "GRADE")
    )

#' From these reliability values, a modified estimate of the half-test
#' reliability is calculated.  Along with these revised reliabilities, the
#' standard deviation of each test is obtained from the observed data and
#' used to calculate the standard error of measurement (SEM).

test_std_dev <-
    Georgia_Data_LONG[
        YEAR %in% 2016:2019,
        .(SD = sd(SCALE_SCORE, na.rm = TRUE)),
        keyby = c("CONTENT_AREA", "GRADE")
    ]

sem_lookup <- test_reliability[test_std_dev]
sem_lookup[,
    Reliability := (0.5 * Reliability)/(1 - (0.5 * Reliability))
][,
    SEM := SD * (sqrt(1 - Reliability))
][,
    c("Reliability", "SD") := NULL
]

setkey(sem_lookup, CONTENT_AREA, GRADE)
setkey(Georgia_Data_LONG, CONTENT_AREA, GRADE)

Georgia_Data_LONG <- sem_lookup[Georgia_Data_LONG]

Georgia_Data_LONG[!is.na(SCALE_SCORE),
    SCALE_SCORE_Short :=
      rnorm(
        n = .N,
        mean = SCALE_SCORE,
        sd = SEM) |> round(0)
]

##  Force scores outside LOSS/HOSS back into range
loss_hoss <-
    list(
      ELA =
        SGPstateData[["GA"]][["Achievement"]][["Knots_Boundaries"]][["ELA.2015"]],
      MATHEMATICS =
        SGPstateData[["GA"]][["Achievement"]][["Knots_Boundaries"]][["MATHEMATICS.2015"]]
    )
for (CA in c("ELA", "MATHEMATICS")) {
    for (G in 3:8) {
        tmp.loss <- loss_hoss[[CA]][[paste0("loss.hoss_", G)]][1]
        tmp.hoss <- loss_hoss[[CA]][[paste0("loss.hoss_", G)]][2]
        Georgia_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short < tmp.loss,
            SCALE_SCORE_Short := tmp.loss
        ]
        Georgia_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short > tmp.hoss,
            SCALE_SCORE_Short := tmp.hoss
        ]
    }
}

#  Summary of ACTUAL scale scores
Georgia_Data_LONG[
    YEAR %in% 2016:2019,
    as.list(round(summary(SCALE_SCORE), 1)),
    keyby = c("CONTENT_AREA", "GRADE")
]

#  Vs. "short"
Georgia_Data_LONG[
    YEAR %in% 2016:2019,
    as.list(round(summary(SCALE_SCORE_Short), 1)),
    keyby = c("CONTENT_AREA", "GRADE")
]

##    Create a "Short" Achievement Level for the "Short" Score

SGP:::getAchievementLevel(
    sgp_data = Georgia_Data_LONG,
    state = "GA",
    # year=NULL,
    # content_area=NULL,
    # grade=NULL,
    achievement.level.name="ACHIEVEMENT_LEVEL_Short",
    scale.score.name="SCALE_SCORE_Short")

# Georgia_Data_LONG[
#     YEAR %in% 2016:2019,
#     as.list(round(summary(SCALE_SCORE_Short), 1)),
#     keyby = c("CONTENT_AREA", "GRADE", "ACHIEVEMENT_LEVEL_Short")
# ]

#' ##  Save data
#'
#+ data-prep-save, echo = TRUE, purl = TRUE, eval = FALSE
setcolorder(
    Georgia_Data_LONG,
    c("VALID_CASE", "SchoolID", gw_var_names,
      "SEM", "SCALE_SCORE_Short", "ACHIEVEMENT_LEVEL_Short")
)

fname <- "Data/Cleaned_Data/Student_LongTestData_Georgia_2016-2022_AVI.csv"
fwrite(Georgia_Data_LONG, file = fname)
zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")

#' ## Summary and notes
#'
#' * Data used for this project was provided by GaDOE (11/2022).
#'   - A subset of 2016-2022 ELA and Math data are retained.
#' * A unique SchoolID variable is created from GA District and School IDs
#' * Variables required by the `SGP` packages were renamed and formatted as
#'   necessary.
#' * Student demographic variables were formatted to be consistent across all
#'   states in the study.
#' * A `VALID_CASE` variable was created and used to identify duplicate records
#'   and other problematic cases.
#' * A second `SCALE_SCORE` variable was created to simulate a half-length test
#'   for condition 4. A corresponding `ACHIEVEMENT_LEVEL` was also created.
