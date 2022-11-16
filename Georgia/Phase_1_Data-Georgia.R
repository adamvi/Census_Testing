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

# Make SchoolID unique - based on SGP code from GA Milestones
Georgia_Data_LONG[,
    SchoolID := as.numeric(SYSTEM_ID)*10000 + as.numeric(SCHOOL_ID)
]
Georgia_Data_LONG[
    which(as.numeric(SYSTEM_ID) > 1000),
      SchoolID := as.numeric(SYSTEM_ID)
]
Georgia_Data_LONG[, SchoolID := as.integer(SchoolID)]


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
          "Multiracial", "Pacific", "Asian", "White")
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

#' ##  Save data
#'
#+ data-prep-save, echo = TRUE, purl = TRUE, eval = FALSE
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
