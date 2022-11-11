#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                   Data Cleaning and Prep -- State A                   ####
####                                                                       ####
###############################################################################

#' #  Data cleaning and preparation
#'
#' For this simulation analysis we will be using the *`sgpData_LONG_COVID`* data
#' from the [`SGPData`](https://github.com/CenterForAssessment/SGPdata) package.
#' It includes 7 years of annual assessment data in two content areas (ELA and
#' Mathematics). As this data is typically used for testing and research
#' purposes with the `SGP` [@sgp] package, much of the data cleaning and
#' formatting has already been done.
#'
#' This section of the appendix assumes the user is operating with their
#' working directory set to the state level directory (e.g., "*./State_A/*".

#+ data-prep-wd, echo = TRUE, purl = TRUE, eval = FALSE
# setwd("./State_A")

#' ## Load packages and custom functions.
#'
#' The following `R` packages are required for the data source, cleaning and
#' augmentation.
#'
#+ data-prep-pkg, echo = TRUE, purl = TRUE
require(SGPdata)
require(data.table)

#' ## General data setup and cleaning
#'
#' This example dataset comes with a "built-in" impact in 2021 related to the
#' pandemic as well as an unperturbed version - *`SCALE_SCORE_without_COVID_IMPACT`*.
#' Here we will first subset the data to include only those years needed for
#' the study, and then remove the perturbed score version and use the original
#' scale score.
#'
#+ data-prep-getdata, echo = TRUE, purl = TRUE
# First load and rename/remove SCALE_SCORE* variables included in the data
State_A_Data_LONG <- copy(SGPdata::sgpData_LONG_COVID)[YEAR < 2020]
State_A_Data_LONG[, SCALE_SCORE := NULL]
setnames(State_A_Data_LONG, "SCALE_SCORE_without_COVID_IMPACT", "SCALE_SCORE")

#' ***NOTE TO LESLIE & EMMA***
#' 
#' We will need to either come to an agreement on the longitudinal data naming
#' or rename according to the `SGP` package conventions. Here I rename the
#' demographic variables to match the "analysis specification" documents and
#' remove some of the variables we will not be looking at or using.
#'
#+ data-prep-rename, echo = TRUE, purl = TRUE
setnames(
  State_A_Data_LONG,
  c("ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS"),
  c("Race", "EconDis", "EL", "SWD")
)
State_A_Data_LONG[, Race := as.character(Race)]
State_A_Data_LONG[Race == "African American", Race := "Black"]
State_A_Data_LONG[, EconDis := gsub("Free Reduced Lunch", "FRL", EconDis)]

State_A_Data_LONG[,
  c("GENDER", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NAME") := NULL
]

#' ## Additional variables for aggregated results
#'
#' A standardized score variable and an achievement proficiency indicator are
#' required for school level aggregations, final analyses and results
#' comparisons. The standardized scale score variable is scaled by each
#' ***year by subject by grade*** test mean and standard deviation^[The original `SCALE_SCORE` variable is used in the SGP calculations.].
#'
#' *NOTE:* I am doing this here, but it could easily be done before the
#' aggregation/summarization step. It is NOT required as any part of the growth
#' analyses.
#' 
#+ data-prep-zscore, echo = TRUE, purl = TRUE
##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
State_A_Data_LONG[,
  Z_SCORE := scale(SCALE_SCORE),
  by = c("YEAR", "CONTENT_AREA", "GRADE")
]

#' A simple '`1/0`' binary indicator for proficiency will allow us to compute
#' descriptive statistics (e.g., percent proficient) easily and consistently
#' across all states included in the report.
#'
#+ data-prep-prof, echo = TRUE, purl = TRUE
##    Proficient/Not (1/0) binary indicator.
State_A_Data_LONG[,
  PROFICIENCY := fcase(
    ACHIEVEMENT_LEVEL %in% c("Partially Proficient", "Unsatisfactory"), 0L,
    ACHIEVEMENT_LEVEL %in% c("Advanced", "Proficient"), 1L
  )
]

State_A_Data_LONG[,
  Z_PROFICIENCY := scale(PROFICIENCY),
  by = c("YEAR", "CONTENT_AREA", "GRADE")
]

#+ data-prep-misc, echo = FALSE, message = FALSE
#   THESE ARE JUST SOME CHECKS THAT CAN BE RUN ON THE Z-SCORE VARIABLES:
# State_A_Data_LONG[,
#   as.list(summary(Z_PROFICIENCY)),
#   keyby = c("YEAR", "CONTENT_AREA", "GRADE")
# ]
# State_A_Data_LONG[,
#     as.list(summary(Z_SCORE)),
#     keyby = c("YEAR", "CONTENT_AREA", "GRADE")
# ]


#' ## Summary and notes
#'
#' * "State A" uses the 2016 to 2019 subset of the *`sgpData_LONG_COVID`*
#'   dataset from the `SGPData` package.
#'   - The "original", unperturbed version of the scaled score is retained.
#' * A standardized scale score variable is added (scaled by unique grade,
#'   content area and annual assessment).
#' * A binary indicator variable for proficiency status is added.
