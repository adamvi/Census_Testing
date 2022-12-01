#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                   Data Cleaning and Prep -- State A                   ####
####                                                                       ####
###############################################################################
if (!dir.exists("./Data/Cleaned_Data"))
    dir.create("./Data/Cleaned_Data", recursive = TRUE)

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
State_A_Data_LONG <- copy(SGPdata::sgpData_LONG_COVID)[YEAR < 2023]


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
  c("ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS",
    "SCHOOL_NUMBER"),
  c("Race", "EconDis", "EL", "SWD", "SchoolID")
)
State_A_Data_LONG[, Race := as.character(Race)]
State_A_Data_LONG[Race == "African American", Race := "Black"]
State_A_Data_LONG[Race == "Other", Race := "Multiracial"]

State_A_Data_LONG[, EconDis := gsub("Free Reduced Lunch", "FRL", EconDis)]
State_A_Data_LONG[, SWD := gsub("IEP", "SWD", SWD)]
State_A_Data_LONG[, EL := gsub("ELL", "EL", EL)]

State_A_Data_LONG[,
  c("SCALE_SCORE_without_COVID_IMPACT", "GENDER",
    "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NAME"
  ) := NULL
]


#' ### Create `VALID_CASE` and invalidate duplicates
#'
#' The `SGP` package requires a variable named `VALID_CASE` with values set to
#' either "VALID_CASE" or "INVALID_CASE". This is helpful in identifying cases
#' that are problematic for any number of reasons (duplicate records, invalid
#' ID types, missing scores or subject/grade values, etc.). Any record flagged
#' as an "INVALID_CASE" will be excluded from any SGP analyses (but remain in
#' the data for possible later use in other aggregations).
#'
#+ data-prep-vc, echo = TRUE, purl = TRUE, eval = FALSE
# Create `VALID_CASE` if it does not exist:
State_A_Data_LONG[, VALID_CASE := "VALID_CASE"]

#  Check for record duplicates:
setkey(State_A_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID, SCALE_SCORE)
setkey(State_A_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID)
# Total Dups:
State_A_Data_LONG |> duplicated(by = key(State_A_Data_LONG)) |> sum()
#  If duplicates, keep the highest score (if any non-NA exist)
dupl <- duplicated(State_A_Data_LONG, by = key(State_A_Data_LONG))
State_A_Data_LONG[which(dupl)-1, VALID_CASE := "INVALID_CASE"]

#  Subset the data to remove invalid student records
State_A_Data_LONG <- State_A_Data_LONG[VALID_CASE == "VALID_CASE"]

#' ##  Save data
#'
#+ data-prep-save, echo = TRUE, purl = TRUE, eval = FALSE
fname <- "Data/Cleaned_Data/Student_LongTestData_State_A_2016-2022_AVI.csv"
fwrite(State_A_Data_LONG, file = fname)
zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")

#' ## Summary and notes
#'
#' * "State A" uses the 2016 to 2022 subset of the *`sgpData_LONG_COVID`*
#'   dataset from the `SGPData` package.
#'   - Only required variables are retained.
#' * Variables required by the `SGP` packages were renamed and formatted as
#'   necessary.
#' * Student demographic variables were formatted to be consistent across all
#'   states in the study.
#' * A `VALID_CASE` variable was created and used to identify duplicate records
#'   and other problematic cases.