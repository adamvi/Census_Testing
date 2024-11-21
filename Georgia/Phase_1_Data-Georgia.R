#+ data-prep, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####                   Data Cleaning and Prep -- Georgia                   ####
####                                                                       ####
###############################################################################
if (!dir.exists("./Data/Phase_1-Cleaned_Data"))
     dir.create("./Data/Phase_1-Cleaned_Data", recursive = TRUE)

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

#' ## Student data setup and cleaning
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
    which(duplicated(Georgia_Data_LONG, by = key(Georgia_Data_LONG))) - 1,
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
           "CONTENT_AREA,  GRADE,  reliability
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
    half_test_rel := (0.5 * reliability)/(1 - (0.5 * reliability))
][,
    SEM := SD * (sqrt(1 - reliability))
][,
    HT_SEM := SD * (sqrt(1 - half_test_rel))
][,
    c("reliability", "half_test_rel", "SD") := NULL
]

setkey(sem_lookup, CONTENT_AREA, GRADE)
setkey(Georgia_Data_LONG, CONTENT_AREA, GRADE)

Georgia_Data_LONG <- sem_lookup[Georgia_Data_LONG]

set.seed(2072)
Georgia_Data_LONG[!is.na(SCALE_SCORE),
    SCALE_SCORE_Short :=
        rnorm(
            n = .N,
            mean = SCALE_SCORE,
            sd = HT_SEM
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
        Georgia_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short_v2 < tmp.loss,
            SCALE_SCORE_Short_v2 := tmp.loss
        ]
        Georgia_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_Short_v2 > tmp.hoss,
            SCALE_SCORE_Short_v2 := tmp.hoss
        ]
        Georgia_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_v2 < tmp.loss,
            SCALE_SCORE_v2 := tmp.loss
        ]
        Georgia_Data_LONG[
          CONTENT_AREA == CA & GRADE == G & SCALE_SCORE_v2 > tmp.hoss,
            SCALE_SCORE_v2 := tmp.hoss
        ]
    }
}

#  Summary of ACTUAL scale scores
Georgia_Data_LONG[
    YEAR %in% 2016:2019,
    as.list(round(summary(SCALE_SCORE), 1)),
    keyby = c("CONTENT_AREA", "GRADE")
]
#  Vs. "retest" imaginationland
Georgia_Data_LONG[
    YEAR %in% 2016:2019,
    as.list(round(summary(SCALE_SCORE_v2), 1)),
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
    achievement.level.name = "ACHIEVEMENT_LEVEL_Short",
    scale.score.name = "SCALE_SCORE_Short"
)
SGP:::getAchievementLevel(
    sgp_data = Georgia_Data_LONG,
    state = "GA",
    achievement.level.name = "ACHIEVEMENT_LEVEL_Short_v2",
    scale.score.name = "SCALE_SCORE_Short_v2"
)
SGP:::getAchievementLevel(
    sgp_data = Georgia_Data_LONG,
    state = "GA",
    achievement.level.name = "ACHIEVEMENT_LEVEL_v2",
    scale.score.name = "SCALE_SCORE_v2"
)
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
      "SCALE_SCORE_v2", "ACHIEVEMENT_LEVEL_v2",
      "SCALE_SCORE_Short", "ACHIEVEMENT_LEVEL_Short",
      "SCALE_SCORE_Short_v2", "ACHIEVEMENT_LEVEL_Short_v2"
    )
)
Georgia_Data_LONG[, c("SEM", "HT_SEM") := NULL]

fname <- "Data/Phase_1-Cleaned_Data/Student_LongTestData_Georgia_2016-2022_AVI.csv"
fwrite(Georgia_Data_LONG, file = fname)
zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")


#' ##  Georgia accountability data

require(tidyverse)
require(janitor)
require(readxl)
require(skimr)

tidyGroupNames = function(df){
    dplyr::mutate(
        .data = df,
        Group =
            fct_recode(
                Group,
                "Native" = "American Indian/Alaskan", 
                "Native" = "American Indian/Alaskan Native", 
                "Multiracial" = "Multi-Racial",
                "SWD" = "Students with Disability",
                "SWD" = "Students With Disability", 
                "EL" = "English Learners", 
                "EconDis" = "Economically Disadvantaged",
                "All" = "ALL Students",
                "Asian" = "Asian/Pacific Islander"
            )
    )
}

##    Load data from MS Excel files
schools_part_rates_18 <-
    read_xlsx(
        path = "Data/03.Participation Rates_2016-2021/Ga_2018_Participation Rates.xlsx",
        na = "No Data Found"
    )
schools_part_rates_19 <-
    read_xlsx(
        path = "Data/03.Participation Rates_2016-2021/Ga_2019_Participation Rates.xlsx",
        na = "No Data Found"
    )
schools_ProgELP_18 <-
    read_xlsx(
        path = "Data/05.Progress in ELP_2016-2021/Ga_2018 CCRPI Progress Scores.xlsx",
        guess_max = 30000,
        na = "NA"
    )
schools_ProgELP_19 <-
    read_xlsx(
        path = "Data/05.Progress in ELP_2016-2021/Ga_2019 CCRPI Progress Scores.xlsx",
        guess_max = 5000,
        na = "NA"
    )
schools_SQSS_18 <-
    read_xlsx(
        path = "Data/06.SQSS_2016-2021/ESSA_Readiness/Ga_2018 CCRPI Readiness Indicators by Subgroup 12_14_18.xlsx",
        guess_max = 200000,
        na = "NA"
    )
schools_SQSS_19 <-
    read_xlsx(
        path = "Data/06.SQSS_2016-2021/ESSA_Readiness/Ga_2019 CCRPI Readiness Indicators Data by Subgroup_10_25_19.xlsx",
        guess_max = 200000,
        na = "NA"
    )

#' ###  Participation data

tidy_participation_data = function(df){
    print(paste("Rows before any cleaning =", nrow(df)))

    # First, for Georgia data, I'll drop the CONTENT_AREAs that aren't Math and ELA
    df <- df  |>
        filter(
            ASSESSMENT_SUBJECT_AREA_CODE %in% c("English Language Arts", "Mathematics")
        ) |>
          filter(GRADE_CLUSTER != "H") |>
            filter(SCHOOL_ID != "ALL")
    print(paste("Rows remaining after filtering =", nrow(df)))

    # After dropping invalid rows, we'll focus on column-level transformations. 
    # As above for the student-level data, we modify SchoolID variable for Georgia
    df <- df |>
        mutate(
            SchoolID =
                case_when(
                    as.numeric(SYSTEM_ID) > 1000 ~ paste0(SYSTEM_ID, GRADE_CLUSTER),
                    TRUE ~ paste0(as.numeric(SYSTEM_ID)*10000 + as.numeric(SCHOOL_ID), GRADE_CLUSTER)
                )
        ) |>
        # Then, I'll drop the columns we aren't interested in:
        select(
            -c(SYSTEM_ID, SCHOOL_ID, SYSTEM_NAME, SCHOOL_NAME, 
               GRADE_CONFIGURATION_CODE, NUMER_TESTED,
               NUMBER_REQUIRED_TO_TEST, SCHOOL_YEAR, GRADE_CLUSTER)
        ) |>
        mutate(PARTICIPATION_RATE = as.numeric(PARTICIPATION_RATE)) |>
        pivot_wider(
            id_cols = c("SchoolID", "REPORTING_CATEGORY_CODE"),
            names_from = ASSESSMENT_SUBJECT_AREA_CODE, 
            values_from = PARTICIPATION_RATE,
            names_prefix = "PartRate"
        )

    # The transform above should halve the number of rows
    print(paste("Final rows remaining pivoting wider =", nrow(df)))

    # We'll rename and rearrange
    df <- df |>
        # rename uses new_name = old_name syntax
        rename(Group = REPORTING_CATEGORY_CODE,
               ELA_PartRate = "PartRateEnglish Language Arts",
               Math_PartRate = PartRateMathematics) |>
        relocate(SchoolID, Group, ELA_PartRate, Math_PartRate) |>
        tidyGroupNames()

    return(df)
}

schools_part_rates_18_tidied <- tidy_participation_data(schools_part_rates_18)
schools_part_rates_19_tidied <- tidy_participation_data(schools_part_rates_19)

rm(schools_part_rates_18, schools_part_rates_19, tidy_participation_data)

#' ###  ELP Progress data

tidy_ProgELP_data = function(df){
    print(paste("Rows before any cleaning =", nrow(df)))

    # Remove the spaces in the column names, capitalize all
    df <- df |>
      clean_names(case = "all_caps") |>
        filter(GRADE_CLUSTER != "H") |>
          filter(SCHOOL_ID != "ALL") |>
            mutate(INDICATOR_SCORE = as.numeric(INDICATOR_SCORE)) |>
              filter(!is.na(INDICATOR_SCORE))
    print(paste("Rows remaining after dropping missing ProgELP values =", nrow(df)))

    # After dropping invalid rows, we'll focus on column-level transformations. 
    # Create our own unique SchoolID variable for Georgia as above
    df <- df |>
        mutate(
            SchoolID =
                case_when(
                    as.numeric(SYSTEM_ID) > 1000 ~ paste0(SYSTEM_ID, GRADE_CLUSTER),
                    TRUE ~ paste0(as.numeric(SYSTEM_ID)*10000 + as.numeric(SCHOOL_ID), GRADE_CLUSTER)
                )
        ) |>
        select(
            -c(SYSTEM_ID, SCHOOL_ID, SYSTEM_NAME, SCHOOL_NAME, GRADE_CLUSTER,
                GRADE_CONFIGURATION, INDICATOR, TARGET, FLAG, SCHOOL_YEAR)
        ) |>
        rename(Group = REPORTING_LABEL,
               ProgELP = INDICATOR_SCORE) |> 
        relocate(SchoolID, Group, ProgELP) |>
        tidyGroupNames()

    return(df)
}

schools_ProgELP_18_tidied <- tidy_ProgELP_data(schools_ProgELP_18)
schools_ProgELP_19_tidied <- tidy_ProgELP_data(schools_ProgELP_19)

rm(schools_ProgELP_18, schools_ProgELP_19, tidy_ProgELP_data)


#' ###  SQSS data

#' For Georgia, we create our own SQSS variable, by taking the mean of the
#' following indicators:
#' 1) Literacy
#' 2) Student Attendance
#' 3) "Beyond the Core"

tidy_SQSS_data = function(df){
    # df <- as.data.table(copy(schools_SQSS_18))
    print(paste("Rows before any cleaning =", nrow(df)))

    # Remove the spaces in the column names, capitalize all
    df <- df |>
      clean_names(case = "all_caps") |>
        filter(GRADE_CLUSTER != "H") |>
          filter(SCHOOL_ID != "ALL") |>
            filter(INDICATOR %in% c("Literacy", "Student Attendance", "Beyond The Core"))
    print(paste("Rows remaining after dropping other indicators =", nrow(df)))

    # Create our own unique SchoolID variable for Georgia as above
    df <- mutate(df, SchoolID =
            case_when(as.numeric(SYSTEM_ID) > 1000  ~ 
                paste0(SYSTEM_ID, GRADE_CLUSTER),
                TRUE ~ 
                paste0(
                    as.numeric(SYSTEM_ID)*10000 +
                    as.numeric(SCHOOL_ID),
                    GRADE_CLUSTER
                )
            )
        ) |>
        select(
            -c(SYSTEM_ID, SCHOOL_ID, SYSTEM_NAME, SCHOOL_NAME,
               GRADE_CLUSTER, GRADE_CONFIGURATION, SCHOOL_YEAR)
        ) |>
        pivot_wider(
            id_cols = c("SchoolID", "REPORTING_LABEL"),
            names_from = INDICATOR, 
            values_from = INDICATOR_SCORE
        )

    print(paste("Final Rows =", nrow(df)))

    # We'll rename and rearrange
    df <- df |>
        clean_names(case = "none") |>
        rename(Group = REPORTING_LABEL) |>
        relocate(SchoolID, Group) |>
        tidyGroupNames() |>
        mutate(
            Beyond_The_Core = as.numeric(Beyond_The_Core),
            Literacy = as.numeric(Literacy),
            Student_Attendance = as.numeric(Student_Attendance)
        )

    return(df)
}

schools_SQSS_18_tidied <- tidy_SQSS_data(schools_SQSS_18)
schools_SQSS_19_tidied <- tidy_SQSS_data(schools_SQSS_19)

rm(schools_SQSS_18, schools_SQSS_19, tidy_SQSS_data, tidyGroupNames)

#' ## Merge across school-level indicators

# Explore missingness before and after the merge:
skim(schools_part_rates_18_tidied)
skim(schools_ProgELP_18_tidied)
skim(schools_SQSS_18_tidied)

# Join all three tables, keeping ALL rows (even if matches aren't found)
georgia_schools_2018 <-
    full_join(schools_part_rates_18_tidied,
                 schools_ProgELP_18_tidied
    ) |>
    left_join(schools_SQSS_18_tidied)
skim(georgia_schools_2018)
rm(schools_part_rates_18_tidied, schools_ProgELP_18_tidied, schools_SQSS_18_tidied)

# Repeat for 2019 data
skim(schools_part_rates_19_tidied)
skim(schools_ProgELP_19_tidied) # Note: 2019 data for ProgELP ONLY reports for the "English Learner" group. 
skim(schools_SQSS_19_tidied)
georgia_schools_2019 <-
    full_join(
        schools_part_rates_19_tidied, 
        schools_ProgELP_19_tidied
    ) |>
    left_join(schools_SQSS_19_tidied)
skim(georgia_schools_2019)
rm(schools_part_rates_19_tidied, schools_ProgELP_19_tidied, schools_SQSS_19_tidied)

## Checking for duplicates

#' Remove all school records with duplicate SchoolID, Level and Group combinations.

#' That is, if two or more records have the same SchoolID, Level and Group values,
#' then all such records should be removed from the analysis.

get_dupes(georgia_schools_2018, SchoolID, Group) |> nrow()
get_dupes(georgia_schools_2019, SchoolID, Group) |> nrow()

##  Calculate SQSS
##
##  Remove cases that will not get Accountability Rating before
##  imputing values and calculating SQSS

georgia_schools_2018 <-
    as.data.table(georgia_schools_2018)[
        !(is.na(ELA_PartRate) | is.na(Math_PartRate))
    ]
georgia_schools_2019 <-
    as.data.table(georgia_schools_2019)[
        !(is.na(ELA_PartRate) | is.na(Math_PartRate))
    ]

# How should NA in indicators be dealt with?
# Impute where missing
sqss.vars <- c("Literacy", "Beyond_The_Core", "Student_Attendance")
for (sqss in sqss.vars) {
    georgia_schools_2018[,
        TMP_M := median(get(sqss), na.rm = TRUE), by = "Group"
    ][is.na(get(sqss)),
        eval(sqss) := round(TMP_M, 3)
    ][, TMP_M := NULL
    ]
}

georgia_schools_2018[,
  SQSS := rowMeans(.SD, na.rm = TRUE),   # SQSS = NA if ALL are NA, mean of those available
  # SQSS := rowSums(.SD, na.rm = FALSE)/3, # SQSS = NA if any are NA?
  # SQSS := rowSums(.SD, na.rm = TRUE)/3,    # Treated as 0? Penalized for NA
  .SDcols = (sqss.vars)
]

# Impute 2019 where missing
for (sqss in sqss.vars) {
    georgia_schools_2019[,
        TMP_M := median(get(sqss), na.rm = TRUE), by = "Group"
    ][is.na(get(sqss)),
        eval(sqss) := round(TMP_M, 3)
    ][, TMP_M := NULL
    ]
  }

georgia_schools_2019[,
  SQSS := rowMeans(.SD, na.rm = TRUE),  #  Keep this option per LK (2/1/23)
  .SDcols = c("Beyond_The_Core", "Literacy", "Student_Attendance")
]

# ProgELP only in "EL" rows in 2019.  Who should this count for? All/EL only? Every Group?
ProgELP_Lookup <-
  georgia_schools_2019[
    Group == "EL" & !is.na(ProgELP),
      .(SchoolID, Group, ProgELP)
  ][,
    Group := "All"
  ]

setkey(ProgELP_Lookup, SchoolID, Group)
setkey(georgia_schools_2019, SchoolID, Group)
georgia_schools_2019 <- ProgELP_Lookup[georgia_schools_2019]
georgia_schools_2019[
  !is.na(i.ProgELP), ProgELP := i.ProgELP
][,
  i.ProgELP := NULL
]



## Sort the cleaned-up school data records, then output the final data. 

georgia_schools_2018 |>
    setcolorder(c("SchoolID", "Group")) |> 
    fwrite(file = "Data/Phase_1-Cleaned_Data/School_AcctData_Georgia_2018_AVI.csv")

georgia_schools_2019 |>
    setcolorder(c("SchoolID", "Group")) |> 
    fwrite(file = "Data/Phase_1-Cleaned_Data/School_AcctData_Georgia_2019_AVI.csv")


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
