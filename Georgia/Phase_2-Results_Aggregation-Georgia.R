#+ res-agr, include = FALSE, echo = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####     2018 and 2019 Growth and Achievement Aggregations for Georgia     ####
####                                                                       ####
###############################################################################
require(data.table)
options(datatable.print.class = FALSE)
options(datatable.print.rownames = FALSE)
options(datatable.print.keys = FALSE)

`%w/o%` = function(x, y) x[!x %in% y]

#  Load student data with all conditions' growth measures
if (!exists("Georgia_Data_LONG")) {
  source("../functions/freadZIP.R")
  Georgia_Data_LONG <-
    freadZIP(
      "Data/Phase_2-Student_Growth/Student_LongTestData_Georgia_2016-2019_AVI.csv.zip"
    )
}
#  Create a Data subdirectory for school aggregated results
if (!dir.exists("./Data/Phase_2-School_Summaries")) dir.create("./Data/Phase_2-School_Summaries")

#' #  Growth and Achievement Aggregations
#'
#' To simplify the analysis and enable comparisons of results across
#' participating states, we plan to simulate a standard "prototype"
#' accountability model with the following features.
#'
#' ***Reporting***
#'
#' * The minimum n-count for computing scores for schools and disaggregated
#'   student groups is varied depending on the simulated condition.
#' * The disaggregated student groups should include economically disadvantaged
#'   students, students from racial and ethnic groups, children with
#'   disabilities, and English learners, as long as they meet the minimum
#'   n-count threshold in the simulated condition.
#'
#' ***Indicators***
#'
#' * **Academic achievement** is the percentage of students in the school
#'   meeting the proficiency in ELA and mathematics (as defined by the
#'  'Proficient' cut score on the statewide assessment).
#' * The computation of the ELA and math proficiency rates are adjusted if a
#'   school or student group does not have at least 95% participation.
#' * For the other academic indicator, we apply the following rules:
#'   - If student-level academic growth (consecutive-year or skip-year) can be
#'     computed, then we will use it for this indicator. For consistency, we
#'     will calculate student growth percentiles (SGPs) using the student-level
#'     assessment data.
#'   - If student-level academic growth (consecutive-year or skip-year) cannot
#'     be computed, then we will use an improvement measure, defined as the
#'     change in average scale scores for each grade-level subject area test
#'     between administrations for the school or student group.
#'
#' ***Summative Rating Computation***
#'
#' All indicator scores are standardized by transforming into z-scores. Use the
#' following means and standard deviations (SD) for the z-score computations
#' (of all schools and student groups):
#'
#' - *Academic achievement*
#'   + Mean: mean student-level proficiency rate for the focus year
#'   + SD: student-level proficiency rate SD for the focus year
#'   + standardized by year, subject and grade.
#' - *Other academic indicator - **growth***
#'   + SGPs, being percentiles, can be converted directly to a standardized
#'     metric^[**Ex. in `R`:** `qnorm(c(1, 10, 25, 50, 75, 90, 99)/100)` gives the z-score for the `r paste(ord(c(1, 10, 25)), collapse = ", ")`, ... etc., percentiles. For more on mapping percentiles on to the standard-normal distribution, [see this site](https://sphweb.bumc.bu.edu/otlt/MPH-Modules/PH717-QuantCore/PH717-Module6-RandomError/PH717-Module6-RandomError7.html)].
#' - *Other academic indicator - **improvement***
#'   + Mean: mean student-level scale score changes for the focus year,
#'     calculated separately for each grade level and subject area
#'   + SD: SD of student-level scale scores for the focus year, calculated
#'     separately for each grade level and subject area
#'   + standardized by year, subject and grade.
#' - *Graduation rates, progress in ELP, and SQSS*
#'   + Mean: mean school-level indicator scores for the focus year
#'   + SD: SD of school-level indicator scores for the focus year
#'
#'
#' ## Additional variables for aggregated results
#'
#' A simple '`1/0`' binary indicator for proficiency will allow us to compute
#' descriptive statistics (e.g., percent proficient) easily and consistently
#' across all states included in the report.
#'
#+ res-agr-prof, echo = TRUE, purl = TRUE
##    Proficient/Not (1/0) binary indicator.
Georgia_Data_LONG[,
  PROFICIENCY := fcase(
    ACHIEVEMENT_LEVEL %in% c("Beginning Learner", "Developing Learner"), 0L,
    ACHIEVEMENT_LEVEL %in% c("Proficient Learner", "Distinguished Learner"), 1L
  )
][,
  PROFICIENCY_C0v2 := fcase(
    ACHIEVEMENT_LEVEL_v2 %in% c("Beginning Learner", "Developing Learner"), 0L,
    ACHIEVEMENT_LEVEL_v2 %in% c("Proficient Learner", "Distinguished Learner"), 1L
  )
][,
  PROFICIENCY_C4 := fcase(
    ACHIEVEMENT_LEVEL_Short %in% c("Beginning Learner", "Developing Learner"), 0L,
    ACHIEVEMENT_LEVEL_Short %in% c("Proficient Learner", "Distinguished Learner"), 1L
  )
][,
  PROFICIENCY_C4v2 := fcase(
    ACHIEVEMENT_LEVEL_Short_v2 %in% c("Beginning Learner", "Developing Learner"), 0L,
    ACHIEVEMENT_LEVEL_Short_v2 %in% c("Proficient Learner", "Distinguished Learner"), 1L
  )
]

# Georgia_Data_LONG[Race == "Pacific", Race := "Asian"]


#' ##  Condition specific summary tables - all students by schools
#'
#' ***NOTE TO LESLIE & EMMA***
#' These tables and aggregations are my first attempts to both interpret and
#' implement what I've read in the "Analysis Specification" document.
#' This is how I would approach this (sub-)Phase given my past experience in
#' aggregating growth and achievement data.
#'
#' The following is an example of a preliminary school-level aggregation table
#' for condition 0. Each condition will have a similar table, generally with
#' only the appropriate `SGP` variable substituted for `SGP_Cnd_0` and changes
#' to the inclusion criteria (`YEAR`, `GRADE` and sometimes `CONTENT_AREA`).
#'
#+ agg-base-cond, echo = TRUE, purl = TRUE, eval = FALSE
sch_summary_cnd_0 <-
    Georgia_Data_LONG[
        YEAR %in% c(2018, 2019) &
        GRADE %in% 3:8,
        .(TotalN = .N,
          ProfN = sum(PROFICIENCY == 1L),
          GrowthN = sum(!is.na(SGP_Cnd_0)),
          MGP = round(mean(SGP_Cnd_0, na.rm = TRUE), 1),
          PctProf = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100
        ),
        keyby = c("SchoolID", "YEAR", "CONTENT_AREA")
    ]

#' Per the specifications doc, this table would be widened to have a column for
#' each subject. This can be achieved with this code (retaining all specified
#' descriptive statistics).

#+ agg-base-cond-format, echo = TRUE, purl = TRUE, eval = FALSE
sch_summary_cnd_0w <-
    dcast(
        data = sch_summary_cnd_0,
        formula = SchoolID + YEAR ~ CONTENT_AREA,
        sep = "..",
        value.var = names(sch_summary_cnd_0) %w/o% key(sch_summary_cnd_0)
    )
setnames(
    sch_summary_cnd_0w,
    sapply(
      names(sch_summary_cnd_0w),
      \(f) {
        tmp.name <- strsplit(f, "[.][.]")[[1]] |> rev() |> paste(collapse = "_")
        gsub("MATHEMATICS", "Math", tmp.name)
      }
    ) |> unlist()
)


#' Creating summary tables for the other conditions would only require changing
#' the data records selected (year, grade and content areas as defined for each
#' condition) and the growth variable specified in the aggregation code above.
#' Adding in the demographic variables is a simple addition of the variable of
#' interest into the `keyby` argument of the `data.table` aggregation. Since we
#' are going to be doing this numerous times, we will use a custom function to
#' create these tables, rather than copying the code for each use case.
#'
#+ agg-sgp-conds-all-stdnt, echo = FALSE, purl = TRUE
source("../functions/schoolAggrGator.R")

school_aggregation_all_students <-
    rbindlist(
      list(
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8],
          growth.var = "SGP_Cnd_0"
        )[, Condition := "0"],
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8],
          growth.var = "SGP_Cnd_0v2"
        )[, Condition := "0v2"],
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% c(3, 5:6, 8)],
          growth.var = "SGP_Cnd_1b"
        )[, Condition := "1b"],
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[
              YEAR %in% c(2018, 2019) &
              (CONTENT_AREA == "ELA" & GRADE %in% c(3, 5, 7) |
              CONTENT_AREA == "MATHEMATICS" & GRADE %in% c(4, 6, 8))
            ],
          growth.var = "SGP_Cnd_1c"
        )[, Condition := "1c"],
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8],
          growth.var = "SGP_Cnd_2"
        )[, Condition := "2"],
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% c(3, 5:6, 8)],
          growth.var = "SGP_Cnd_3"
        )[, Condition := "3"],
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8],
          growth.var = "SGP_Cnd_4"
        )[, Condition := "4"],
        schoolAggrGator(
          data_table =
            Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8],
          growth.var = "SGP_Cnd_4v2"
        )[, Condition := "4v2"]
      )
    )[, Group := "All"] |>
      setcolorder(c("Condition", "SchoolID", "Group", "YEAR")) |>
      setkeyv(c("Condition", "YEAR", "SchoolID"))


#' You may notice that there are more summary calculations than what will be
#' used (e.g., percent proficient and mean standardized scale scores). Those
#' are included for our review - so we can easily see what a z-score, of for
#' example 0.5, corresponds to in the actual percent proficient or mean SGP.
#' Here are two schools from the condition 0 table:
#'
#+ agg-cond-0-ex, echo = TRUE, purl = TRUE
# school_aggregation_all_students[
#     Condition == 0 & SchoolID == `SCHOOL-XYZ`, # rows to keep
#     c(key(school_aggregation_all_students)[-1],
#       grep("ELA", names(school_aggregation_all_students), value = TRUE)
#     ),  #  columns to keep
#     with = FALSE
#   ] |>
#     setkey(SchoolID) |> print()

#' We can look at these tables in a number of ways to make sure we are getting
#' what is expected.  A simply cross-tab by year shows that many schools do not
#' get a summary in condition 1c given the testing pattern:
#'
#+ agg-all-inspect, echo = TRUE, purl = TRUE
table(school_aggregation_all_students[, .(Condition, YEAR), is.na(Math_MGP)])

fwrite(school_aggregation_all_students,
    file = "Data/Phase_2-School_Summaries/School_Condition_AllGrowth_AllStudents_Georgia_AllYears_AVI.csv"
)

#' ##  Achievement Improvement Aggregations
#'
#' The simulation condition 1a does not allow for growth calculations and will
#' instead use an indicator of status improvement. This **improvement** measure
#' is defined as the change in average scale scores for each grade-level content
#' area test between administrations for the school or student group.
#'
#' For this aggregation we will create status summaries in a similar way as the
#' other conditions, but include all available years. Lagged values are then
#' created and the change scores calculated.
#'
#+ agg-improve, echo = TRUE, purl = TRUE, eval = FALSE
sch_summary_cnd_1a <-
    Georgia_Data_LONG[
        GRADE %in% c(5, 8),
        .(N = sum(!is.na(SCALE_SCORE)),
          ProfN = sum(PROFICIENCY == 1L),
          MeanScore = mean(SCALE_SCORE, na.rm = TRUE),
          ScoreSD = sd(SCALE_SCORE, na.rm = TRUE)
        ),
        keyby = c("YEAR", "CONTENT_AREA", "GRADE", "SchoolID")
    ]

#  Create lagged variables (1 year lag):
setkeyv(
  sch_summary_cnd_1a,
  c("SchoolID", "CONTENT_AREA", "YEAR", "GRADE")
)
cfaTools::getShiftedValues(
    sch_summary_cnd_1a,
    shift_group = c("SchoolID", "CONTENT_AREA"),
    shift_variable = c("N", "MeanScore", "ScoreSD"),
    shift_amount = 1L
)

#  Subset the data for the two focus years:
sch_summary_cnd_1a <-
    sch_summary_cnd_1a[YEAR %in% c(2018, 2019)]

#  Calculate Change Score (Z - effect size?)
# ELA_G5_ZDiff = (ELA_G5AvgScore_<FYear> - ELA_G5AvgScore_<PYear>) / [(ELA_G5StdDev_<FYear>  ELA_G5N_<FYear> + ELA_G5StdDev_<PYear>  ELA_G5N_<PYear>) / (ELA_G5N_<FYear> + ELA_G5N_<PYear>)]

sch_summary_cnd_1a[,
    ZDiff := (MeanScore - MeanScore_LAG_1) / ((ScoreSD*N + ScoreSD_LAG_1*N_LAG_1)/(N + N_LAG_1))
]


#' Here is our example school's improvement numbers
# sch_summary_cnd_1a[SchoolID == `SCHOOL-XYZ`,
#     c(key(sch_summary_cnd_1a)[-1], "ZDiff"), with = FALSE
# ]


#' One important factor to consider with the "school improvement" indicator is
#' the issue of "regression to the mean" that is expected to occur. There are
#' a strong relationships between current/prior scores and change scores that
#' may require correction:
#'
#+ agg-1b-rtm, echo = TRUE, purl = TRUE
cor(
  sch_summary_cnd_1a[, MeanScore, ZDiff],
  use = 'na.or.complete'
) |> round(3)

cor(
  sch_summary_cnd_1a[, MeanScore_LAG_1, ZDiff],
  use = 'na.or.complete'
) |> round(3)

sch_summary_cnd_1a[,
  .(Current_Year = cor(MeanScore, ZDiff, use = 'na.or.complete'),
    Prior_Year = cor(MeanScore_LAG_1, ZDiff, use = 'na.or.complete')
  ),
  keyby = c("YEAR", "CONTENT_AREA", "GRADE")
]

#' Per the specifications doc, here is how we could reshape this summary table
#' so that there are separate columns for each grade and subject.
#'
#+ agg-improve-format, echo = TRUE, purl = TRUE
#  Reshape by subject
sch_summary_cnd_1a[, GRADE := paste0("G", GRADE)]
setkeyv(
  sch_summary_cnd_1a,
  c("SchoolID", "CONTENT_AREA", "YEAR", "GRADE")
)
sch_summary_cnd_1a <-
    dcast(
        data = sch_summary_cnd_1a,
        formula = SchoolID + YEAR ~ GRADE + CONTENT_AREA,
        sep = "..",
        value.var = c("N", "ProfN", "ZDiff")
    )
setnames(
    sch_summary_cnd_1a,
    sapply(
      names(sch_summary_cnd_1a),
      \(f) {
        tmp.name <- strsplit(f, "[.][.]")[[1]] |> rev() |> paste(collapse = "_")
        gsub("MATHEMATICS", "Math", tmp.name)
      }
    ) |> unlist()
)

#' If schools have both 5th and 8th grades, then take a weighted average of
#' their <Subject>_G5_ZDiff and <Subject>_G8_ZDiff to create the ELA_Improve and
#' Math_Improve variable, otherwise <Subject>_Improve is equal to the grade
#' level <Subject>_G5_ZDiff or <Subject>_G8_ZDiff they DO have in the school.
#'
#' NOTE: This is not necessary if we break up the schools into Elementary and
#'       Middle grade only schools.
#'
#+ agg-improve-var, echo = TRUE, purl = TRUE
sch_summary_cnd_1a[,
  ELA_Improve := ELA_G5_ZDiff
][
  is.na(ELA_Improve),
  ELA_Improve := ELA_G8_ZDiff
][
  !is.na(ELA_G5_ZDiff) & !is.na(ELA_G8_ZDiff),
  ELA_Improve :=
    ((ELA_G5_ZDiff * ELA_G5_N) + (ELA_G8_ZDiff * ELA_G8_N))/(ELA_G5_N + ELA_G8_N)
][,
  Math_Improve := Math_G5_ZDiff
][
  is.na(Math_Improve),
  Math_Improve := Math_G8_ZDiff
][
  !is.na(Math_G5_ZDiff) & !is.na(Math_G8_ZDiff),
  Math_Improve :=
    ((Math_G5_ZDiff * Math_G5_N) + (Math_G8_ZDiff * Math_G8_N))/(Math_G5_N + Math_G8_N)
][,
  ELA_TotalN := rowSums(.SD, na.rm = TRUE),
  .SDcols = c("ELA_G5_N", "ELA_G8_N")
][,
  Math_TotalN := rowSums(.SD, na.rm = TRUE),
  .SDcols = c("Math_G5_N", "Math_G8_N")
][,
  ELA_ProfN := rowSums(.SD, na.rm = TRUE),
  .SDcols = c("ELA_G5_ProfN", "ELA_G8_ProfN")
][,
  Math_ProfN := rowSums(.SD, na.rm = TRUE),
  .SDcols = c("Math_G5_ProfN", "Math_G8_ProfN")
]

sch_summary_cnd_1a[, (grep("G5|G8", names(sch_summary_cnd_1a))) := NULL]


#' Finally, we add the `Group` variable to indicate this data set is for all
#' students, and save files for each year.
#'
#+ agg-improve-save, echo = TRUE, purl = TRUE
sch_summary_cnd_1a[, Group := "All"]
setcolorder(sch_summary_cnd_1a, c("YEAR", "SchoolID", "Group"))

fwrite(sch_summary_cnd_1a[YEAR == 2018][, YEAR := NULL],
    file = "Data/Phase_2-School_Summaries/School_Condition_1a_Georgia_2018_AVI.csv"
)
fwrite(sch_summary_cnd_1a[YEAR == 2019][, YEAR := NULL],
    file = "Data/Phase_2-School_Summaries/School_Condition_1a_Georgia_2019_AVI.csv"
)

#' Note that the above code can be accomplished with the `cond1aAggrGator`
#' function:
sch_smry_cnd_1a <- cond1aAggrGator(data_table = Georgia_Data_LONG)

#' This function is used below for the condition 1a summaries that are
#' disaggregated by demographic subgroups.
#'
#'
#' ##  School level aggregations by demographics
#'
#' Adding in the demographic variables is a simple addition of the variable of
#' interest into the `keyby` argument of the `data.table` aggregation.
#'
#' In order to do all the demographic summaries at once, we can combine calls
#' to the function (rather than creating separate tables and THEN combining)
#' using the `rbindlist` function from `data.table`.
#' Note that the demographic subgroup indicator is changed (from the demographic
#' variable name) to the generic "Group" for each individual table.:
#'
#' All of the demographic aggregation and combination code chunks can be run
#' for each of the `SGP_Cnd*` growth fields. At that point, we can then combine
#' those objects in a wide format, stacked into a long format (with an added
#' "Condition" variable for each table) or written to separate .csv files as
#' described in the specification doc.
#'
#' The code below creates a stacked long format version with all
#' conditions that contain growth.
#'
#+ agg-stdnt-demog, echo = FALSE, purl = TRUE
school_aggregation_student_demogs <-
    rbindlist(
      list(
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
              growth.var = "SGP_Cnd_0",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "0"] |> setnames(f, "Group")
          }
        ) |> rbindlist(),
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
              growth.var = "SGP_Cnd_0v2",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "0v2"] |> setnames(f, "Group")
          }
        ) |> rbindlist(),
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[
                  YEAR %in% c(2018, 2019) & GRADE %in% c(3, 5:6, 8), ],
              growth.var = "SGP_Cnd_1b",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "1b"] |> setnames(f, "Group")
          }
        ) |> rbindlist(),
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[
                  YEAR %in% c(2018, 2019) &
                  (CONTENT_AREA == "ELA" & GRADE %in% c(3, 5, 7) |
                   CONTENT_AREA == "MATHEMATICS" & GRADE %in% c(4, 6, 8))
                ],
              growth.var = "SGP_Cnd_1c",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "1c"] |> setnames(f, "Group")
          }
        ) |> rbindlist(),
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
              growth.var = "SGP_Cnd_2",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "2"] |> setnames(f, "Group")
          }
        ) |> rbindlist(),
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[
                  YEAR %in% c(2018, 2019) & GRADE %in% c(3, 5:6, 8), ],
              growth.var = "SGP_Cnd_3",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "3"] |> setnames(f, "Group")
          }
        ) |> rbindlist(),
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[
                  YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
              growth.var = "SGP_Cnd_4",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "4"] |> setnames(f, "Group")
          }
        ) |> rbindlist(),
        lapply(
          c("Race", "EconDis", "EL", "SWD"),
          \(f) {
            schoolAggrGator(
              data_table =
                Georgia_Data_LONG[
                  YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
              growth.var = "SGP_Cnd_4v2",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "4v2"] |> setnames(f, "Group")
          }
        ) |> rbindlist()
      )
    ) |>
      setcolorder("Condition")

fwrite(school_aggregation_student_demogs,
    file = "Data/Phase_2-School_Summaries/School_Condition_AllGrowth_Demographics_Georgia_AllYears_AVI.csv"
)


#' We can obtain the condition 1a by demographic subgroup using the following
#' call to the `cond1aAggrGator` function with `group` argument supplied via
#' `lapply`:

sch_summary_cnd_1a_demogs <-
  lapply(
    c("Race", "EconDis", "EL", "SWD"),
    \(f) {
      cond1aAggrGator(
        data_table = Georgia_Data_LONG,
        group = f
      )
    }
  ) |>
    rbindlist()

#' ##  A single file with all aggregations
#'
#' I may be missing something about why all the various .csv files are needed
#' for this step, but if we wanted to save all the conditions and summaries as
#' a single file, we could do something like this (and then save as a single
#' .csv using `fwrite` or similar):

#+ res-agr-save-all, echo = TRUE, purl = FALSE
setcolorder(
    school_aggregation_all_students,
    names(school_aggregation_student_demogs)
)
school_aggregation_all <-
    rbindlist(
        list(
          school_aggregation_all_students,
          school_aggregation_student_demogs
        )
    )
setkeyv(school_aggregation_all, c("Condition", "SchoolID", "YEAR"))

fwrite(school_aggregation_all,
    file = "Data/Phase_2-School_Summaries/School_Condition_AllGrowth_Demographics_Georgia_AllYears_AVI.csv"
)


#' ###  Save the condition/year specific aggregation files
#'
#' Here is how to split up the single file created above into the individual
#' files per the specification document.
#'
#+ res-agr-save-sep, echo = TRUE, purl = FALSE
school_aggregation_all[
  Condition %in% c("2", "3") & YEAR == 2018,
  Condition := paste0(Condition, "_E")
][
  Condition %in% c("2", "3") & YEAR == 2019,
  Condition := paste0(Condition, "_O")
]

fprefix <- "./Data/Phase_2-School_Summaries/School_Condition_"

for (cond in c("0", "0v2", "1a", "1b", "1c", "2_E", "2_O", "3_E", "3_O", "4", "4v2")) {
  for (yr in 2018:2019) {
    tmp.tbl <-
      school_aggregation_all[
        Condition == cond & YEAR == yr
      ][,
        c("Condition", "YEAR") := NULL
      ][
        -(grep(": No", Group)),
      ][,
        Group := gsub(": Yes", "", Group)
      ]

##  School_Condition_<c> _<State>_<FYear>_ <init>.csv
    if (nrow(tmp.tbl) > 0L) {
      fwrite(
        x = tmp.tbl,
        file = paste0(fprefix, cond, "_Georgia_", yr, "_AVI.csv")
      )
    }
  }
}

#' Repeat this process of combining, sub-setting and saving for condition 1a.
#' 
#+ c1a-agr-save-sep, echo = TRUE, purl = FALSE
sch_summary_cnd_1a_all <-
    rbindlist(
        list(
          sch_summary_cnd_1a,
          sch_summary_cnd_1a_demogs
        )
    )[
      -(grep(": No", Group)),
    ][,
      Group := gsub(": Yes", "", Group)
    ]

fwrite(sch_summary_cnd_1a_all[YEAR == 2018][, YEAR := NULL],
    file = "Data/Phase_2-School_Summaries/School_Condition_1a_Georgia_2018_AVI.csv"
)
fwrite(sch_summary_cnd_1a_all[YEAR == 2019][, YEAR := NULL],
    file = "Data/Phase_2-School_Summaries/School_Condition_1a_Georgia_2019_AVI.csv"
)

#' ##  Summary and notes
#'
#' * A binary indicator variable for proficiency status is added.
#' * Methods for using the `data.table` package for calculating school level
#'   aggregations for all students and by demographic subgroups were outlined
#'   and discussed with examples.