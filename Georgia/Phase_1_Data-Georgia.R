#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                   Data Cleaning and Prep -- Georgia                   ####
####                                                                       ####
###############################################################################

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
require(data.table)

#' ## General data setup and cleaning
#'
#' Things we need to do...
#'
#+ data-prep-getdata, echo = TRUE, purl = TRUE
# First load and rename/remove SCALE_SCORE* variables included in the data


#+ data-prep-rename, echo = TRUE, purl = TRUE
# setnames(
#   Georgia_Data_LONG,
#   c("ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS"),
#   c("Race", "EconDis", "EL", "SWD")
# )
# Georgia_Data_LONG[, Race := as.character(Race)]
# Georgia_Data_LONG[Race == "African American", Race := "Black"]
# Georgia_Data_LONG[, EconDis := gsub("Free Reduced Lunch", "FRL", EconDis)]

Georgia_Data_LONG[,
  c("ASSESSMENT_TYPE_CODE", "LEXILE_SCALE_SCORE", "SCHOOL_NAME") := NULL
]

#' ## Additional variables for aggregated results
#'
#' A standardized score variable and an achievement proficiency indicator are
#' required for school level aggregations, final analyses and results
#' comparisons. The standardized scale score variable is scaled by each
#' ***year by subject by grade*** test mean and standard deviation^[The unstandardized `SCALE_SCORE` variable is used in the SGP calculations.].
#'
#' *NOTE:* I am doing this here, but it could easily be done before the
#' aggregation/summarization step. It is NOT required as any part of the growth
#' analyses.
#' 
#+ data-prep-zscore, echo = TRUE, purl = TRUE
##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
Georgia_Data_LONG[,
  Z_SCORE := scale(SCALE_SCORE),
  by = c("YEAR", "CONTENT_AREA", "GRADE")
]

#' A simple '`1/0`' binary indicator for proficiency will allow us to compute
#' descriptive statistics (e.g., percent proficient) easily and consistently
#' across all states included in the report.
#'
#+ data-prep-prof, echo = TRUE, purl = TRUE
##    Proficient/Not (1/0) binary indicator.
Georgia_Data_LONG[,
  PROFICIENCY := fcase(
    ACHIEVEMENT_LEVEL %in% c("Partially Proficient", "Unsatisfactory"), 0L,
    ACHIEVEMENT_LEVEL %in% c("Advanced", "Proficient"), 1L
  )
]

Georgia_Data_LONG[,
  Z_PROFICIENCY := scale(PROFICIENCY),
  by = c("YEAR", "CONTENT_AREA", "GRADE")
]

#+ data-prep-misc, echo = FALSE, message = FALSE
#   THESE ARE JUST SOME CHECKS THAT CAN BE RUN ON THE Z-SCORE VARIABLES:
# Georgia_Data_LONG[,
#   as.list(summary(Z_PROFICIENCY)),
#   keyby = c("YEAR", "CONTENT_AREA", "GRADE")
# ]
# Georgia_Data_LONG[,
#     as.list(summary(Z_SCORE)),
#     keyby = c("YEAR", "CONTENT_AREA", "GRADE")
# ]


#' ## Summary and notes
#'
#' * Data used for this project was provided by GaDOE (11/2022).
#'   - A subset of 2016-2019 ELA and Math data are retained.
#' * A standardized scale score variable is added (scaled by unique grade,
#'   content area and annual assessment).
#' * A binary indicator variable for proficiency status is added.
