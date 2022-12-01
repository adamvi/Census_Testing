#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                   Data Cleaning and Prep -- Illinois                   ####
####                                                                       ####
###############################################################################
if (!dir.exists("./Data/Cleaned_Data"))
     dir.create("./Data/Cleaned_Data", recursive = TRUE)

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
        (c(1:7, 14, 16, 21:23)),
        with = FALSE
    ]

#+ data-prep-rename, echo = TRUE, purl = TRUE, eval = FALSE
gw_var_names <-
    c("Race", "SWD", "EconDis", "EL", "SchoolID")

setnames(
  Illinois_Data_LONG,
  old = c("FederalRaceEthnicity", "StudentWithDisabilities",
          "EconomicDisadvantageStatus", "EnglishLearnerEL", "SCHOOL_NUMBER"
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
               labels = c("Other", "Asian", "Black", "LatinX",
                          "Other", "White", "Other", "Other")
        )
]
Illinois_Data_LONG[, Race := as.character(Race)]
# 2 = "Asian"
# 3 = "Black"
# 4 = "LatinX"
# 6 = "White"

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
    which(duplicated(Illinois_Data_LONG, by=key(Illinois_Data_LONG)))-1,
      VALID_CASE := "INVALID_CASE"
]
table(Illinois_Data_LONG[, .(YEAR, VALID_CASE), GRADE])
# Illinois_Data_LONG <- Illinois_Data_LONG[VALID_CASE == "VALID_CASE"]

#' ##  Save data
#'
#+ data-prep-save, echo = TRUE, purl = TRUE, eval = FALSE
fname <- "Data/Cleaned_Data/Student_LongTestData_Illinois_2016-2019_AVI.csv"
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
