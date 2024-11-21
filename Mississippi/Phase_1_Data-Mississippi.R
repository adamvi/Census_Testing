#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                 Data Cleaning and Prep -- Mississippi                 ####
####                                                                       ####
###############################################################################
if (!dir.exists("./Data/Cleaned_Data"))
     dir.create("./Data/Cleaned_Data", recursive = TRUE)

#' #  Data merging, cleaning and preparation
#'
#' For this simulation analysis we will be using data provided by the Mississippi
#' Department of Education (MSDOE) for this project...
#'
#' This section of the appendix assumes the user is operating with their
#' working directory set to the state level directory (e.g., "*./Mississippi/*".

#+ data-prep-wd, echo = TRUE, purl = TRUE, eval = FALSE
# setwd("./Mississippi")

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
# Load raw GA data
Mississippi_Data_LONG <-
    rbindlist(
        list(
            fread("Data/17-18 ACCESSMENT DATA-73637.txt", sep = "|"),
            fread("Data/18-19 ACCESSMENT DATA-73637.txt", sep = "|"),
            fread("Data/20-21 ACCESSMENT DATA-73637.txt", sep = "|"),
            fread("Data/21-22 ACCESSMENT DATA-73637 (1).txt", sep = "|")
        )
    )

##    CONTENT_AREA must match meta-data in `SGPstateData`
Mississippi_Data_LONG[, TEST_TYPE := gsub("MAP ", "", TEST_TYPE)]
Mississippi_Data_LONG[TEST_TYPE == "MATH", TEST_TYPE := "MATHEMATICS"]

# Best to get this out of the way ASAP - 3.65 mill extra records
Mississippi_Data_LONG <-
    Mississippi_Data_LONG[
        TEST_TYPE %in% c("ELA", "MATHEMATICS") &
        GRADE %in% 3:8,
    ]

# Make SchoolID unique - based on SGP code from GA Milestones
Mississippi_Data_LONG[,
    SchoolID := as.numeric(DISTRICT_NUMBER)*10000 + as.numeric(SCHOOL_NUMBER)
]
Mississippi_Data_LONG[, SchoolID := as.character(SchoolID)]
Mississippi_Data_LONG[GRADE %in% 3:5, SchoolID := paste0(SchoolID, "E")]
Mississippi_Data_LONG[GRADE %in% 6:8, SchoolID := paste0(SchoolID, "M")]

#+ data-prep-rename, echo = TRUE, purl = TRUE, eval = FALSE
gw_var_names <-
    c("YEAR", "ID", "CONTENT_AREA", "GRADE",
      "Race", "SWD",# "EconDis", "EL",
      "SCALE_SCORE", "ACHIEVEMENT_LEVEL"
    )

setnames(
  Mississippi_Data_LONG,
  old = c("YEAR", "RESEARCH_ID", "TEST_TYPE", "GRADE",
          "RACE", "SPED", # "ECON_DISADVANTAGE", "EL",
          "SCALE_SCORE", "PROFICIENCY_SCORE"
        ),
  new = gw_var_names
)
setcolorder(Mississippi_Data_LONG, gw_var_names)


#' ### Tiddy up `SGP` required variables and student demographics
#'
#+ data-prep-tidy, echo = TRUE, purl = TRUE, eval = FALSE
Mississippi_Data_LONG[, SCALE_SCORE := as.numeric(SCALE_SCORE)]

Mississippi_Data_LONG[, ACHIEVEMENT_LEVEL := as.factor(ACHIEVEMENT_LEVEL)]
setattr(
    Mississippi_Data_LONG$ACHIEVEMENT_LEVEL, "levels",
    c("Minimal", "Basic", "Passing",
      "Proficient", "Advanced"
    )
)
Mississippi_Data_LONG[, ACHIEVEMENT_LEVEL := as.character(ACHIEVEMENT_LEVEL)]

Mississippi_Data_LONG[
    CONTENT_AREA == "E", CONTENT_AREA := "ELA"
][
    CONTENT_AREA == "M", CONTENT_AREA := "MATHEMATICS"
]

##    Remove extraneous variables
Mississippi_Data_LONG[, c("IEPIND", "FIVEOFOUR", "FEMALE") := NULL]


##    Demographics
# Mississippi_Data_LONG[, Race := factor(Race)]
# setattr(Mississippi_Data_LONG$Race, "levels",
#         c("Asian", "Black", "Hispanic", "Native American",
#           "Pacific Islander", "Multiracial", "White")
#         )
# Mississippi_Data_LONG[, Race := as.character(Race)]
# Mississippi_Data_LONG[,
#     EconDis := ifelse(EconDis == "N", "EconDis: No", "EconDis: Yes")
# ]
# Mississippi_Data_LONG[, EL := ifelse(EL == "N", "EL: No", "EL: Yes")]
Mississippi_Data_LONG[, SWD := ifelse(SWD == "N", "SWD: No", "SWD: Yes")]

#' ### Create `VALID_CASE` and invalidate duplicates
#+ data-prep-vc, echo = TRUE, purl = TRUE, eval = FALSE
Mississippi_Data_LONG[, VALID_CASE := "VALID_CASE"]
setkey(Mississippi_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID, SCALE_SCORE)
setkey(Mississippi_Data_LONG,
    VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID)
Mississippi_Data_LONG |> duplicated(by = key(Mississippi_Data_LONG)) |> sum()
# 3803 duplicates - (((take the highest score if any exist)))
# dupl <- duplicated(Mississippi_Data_LONG, by = key(Mississippi_Data_LONG))
Mississippi_Data_LONG[
    which(duplicated(Mississippi_Data_LONG, by=key(Mississippi_Data_LONG)))-1,
      VALID_CASE := "INVALID_CASE"
]
table(Mississippi_Data_LONG[, .(YEAR, VALID_CASE), GRADE])
Mississippi_Data_LONG <- Mississippi_Data_LONG[VALID_CASE == "VALID_CASE"]

#' ##  Save data
#'
#+ data-prep-save, echo = TRUE, purl = TRUE, eval = FALSE
fname <- "Data/Cleaned_Data/Student_LongTestData_Mississippi_2016-2022_AVI.csv"
fwrite(Mississippi_Data_LONG, file = fname)
zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")

#' ## Summary and notes
#'
#' * Data used for this project was provided by MSDOE (12/2022).
#'   - A subset of 2017-2021 ELA and Math data are retained.
#' * A unique SchoolID variable is created from District and School IDs
#' * Variables required by the `SGP` packages were renamed and formatted as
#'   necessary.
#' * Student demographic variables were formatted to be consistent across all
#'   states in the study.
#' * A `VALID_CASE` variable was created and used to identify duplicate records
#'   and other problematic cases.
