#+ res-agr, include = FALSE, echo = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####     2018 and 2019 Growth and Achievement Aggregations for State A     ####
####                                                                       ####
###############################################################################
require(data.table)
options(datatable.print.class = FALSE)
options(datatable.print.rownames = FALSE)
options(datatable.print.keys = FALSE)

`%w/o%` = function(x, y) x[!x %in% y]

#  Load student data with all conditions' growth measures
if (!exists("State_A_Data_LONG")) {
  source("../functions/freadZIP.R")
  State_A_Data_LONG <-
    freadZIP(
      "Data/Student_Growth/Student_LongTestData_State_A_2016-2019_AVI.csv.zip"
    )
}
#  Create a Data subdirectory for school aggregated results
if (!dir.exists("./Data/School_Summaries")) dir.create("./Data/School_Summaries")


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
#'   + SD:  student-level proficiency rate SD for the focus year
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
#' ## Additional variables for aggregated results
#'
#' A standardized score variable and an achievement proficiency indicator are
#' required for school level aggregations, final analyses and results
#' comparisons. The standardized scale score variable is scaled by each
#' ***year by subject by grade*** test mean and standard deviation^[The unstandardized `SCALE_SCORE` variable is used in the SGP calculations.].
#'
#+ res-agr-zscore, echo = TRUE, purl = TRUE
##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
State_A_Data_LONG[,
  Z_SCORE := scale(SCALE_SCORE),
  by = c("YEAR", "CONTENT_AREA", "GRADE")
]

#' A simple '`1/0`' binary indicator for proficiency will allow us to compute
#' descriptive statistics (e.g., percent proficient) easily and consistently
#' across all states included in the report.
#'
#+ res-agr-prof, echo = TRUE, purl = TRUE
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

#+ res-agr-misc, echo = FALSE, message = FALSE
#   THESE ARE JUST SOME CHECKS THAT CAN BE RUN ON THE Z-SCORE VARIABLES:
# State_A_Data_LONG[,
#   as.list(summary(Z_PROFICIENCY)),
#   keyby = c("YEAR", "CONTENT_AREA", "GRADE")
# ]
# State_A_Data_LONG[,
#     as.list(summary(Z_SCORE)),
#     keyby = c("YEAR", "CONTENT_AREA", "GRADE")
# ]


#' ##  Condition specific summary tables - all students by schools
#'
#' ***NOTE TO LESLIE & EMMA***
#' These tables and aggregations (as well as my variable additions such as
#' `Z_PROFICIENCY` and `Z_SCORE`) are my first attempts to
#' both interpret and implement what I've read in the "Analysis Specification"
#' document. Since I have some extensive experience in aggregating growth and
#' achievement data, this is how I would approach it at this early stage...
#' 
#' The following is an example of a preliminary school-level aggregation table
#' for condition 0. Each condition will have a similar table, generally with
#' only the appropriate `SGP` variable substituted for `SGP_Cnd_0` and changes
#' to the inclusion criteria (`YEAR`, `GRADE` and sometimes `CONTENT_AREA`).
#'
#+ agg-base-cond, echo = TRUE, purl = TRUE
sch_summary_cnd_0 <-
    State_A_Data_LONG[
        YEAR %in% c(2018, 2019) &
        GRADE %in% 3:8,
        .(TotalN = .N,
          ProfN = sum(PROFICIENCY==1L),
          GrowthN = sum(!is.na(SGP_Cnd_0)),
          MGP = round(mean(SGP_Cnd_0, na.rm = TRUE), 1),
          MeanScore = round(mean(Z_SCORE, na.rm = TRUE), 2),
          PcntProf = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          StatusZ = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          GrowthZ = round(mean(qnorm(SGP_Cnd_0/100), na.rm = TRUE), 3)
        ),
        keyby = c("SchoolID", "YEAR", "CONTENT_AREA")
    ]

#' Per the specifications doc, this table would be widened to have a column for
#' each subject. This can be achieved with this code (retaining all specified
#' and my additional descriptive statistics).

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

#   Write files for each year separately as per the specifications:
fwrite(sch_summary_cnd_0w[YEAR == 2018][, YEAR := NULL],
    file = "Data/School_Summaries/School_Condition_0_State_A_2018_AVI.csv"
)
fwrite(sch_summary_cnd_0w[YEAR == 2019][, YEAR := NULL],
    file = "Data/School_Summaries/School_Condition_0_State_A_2019_AVI.csv"
)

#+ agg-sim-conds, echo = FALSE, purl = TRUE
sch_summary_cnd_1b <-
    State_A_Data_LONG[
        YEAR %in% c(2018, 2019) &
        GRADE %in% c(3, 5:6, 8),
        .(TotalN_1b = .N,
          ProfN_1b = sum(PROFICIENCY==1L),
          GrowthN_1b = sum(!is.na(SGP_Cnd_1b)),
          MGP_1b = round(mean(SGP_Cnd_1b, na.rm = TRUE), 1),
          MeanScore_1b = round(mean(Z_SCORE, na.rm = TRUE), 2),
          PcntProf_1b = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          StatusZ_1b = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          GrowthZ_1b = round(mean(qnorm(SGP_Cnd_1b/100), na.rm = TRUE), 3)
          # T_Growth = round(mean(qt(SGP_Cnd_1b/100, df = .N-1), na.rm = TRUE), 3), # t distribution vs normal
        ),
        keyby = c("SchoolID", "YEAR", "CONTENT_AREA")
    ]

sch_summary_cnd_1c <-
    State_A_Data_LONG[
        YEAR %in% c(2018, 2019) &
        (CONTENT_AREA == "ELA" & GRADE %in% c(3, 5, 7) |
         CONTENT_AREA == "MATHEMATICS" & GRADE %in% c(4, 6, 8)
        ),
        .(TotalN_1c = .N,
          ProfN_1c = sum(PROFICIENCY==1L),
          GrowthN_1c = sum(!is.na(SGP_Cnd_1c)),
          MGP_1c = round(mean(SGP_Cnd_1c, na.rm = TRUE), 1),
          MeanScore_1c = round(mean(Z_SCORE, na.rm = TRUE), 2),
          PcntProf_1c = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          StatusZ_1c = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          GrowthZ_1c = round(mean(qnorm(SGP_Cnd_1c/100), na.rm = TRUE), 3)
        ),
        keyby = c("SchoolID", "YEAR", "CONTENT_AREA")
        # substitute `by` arg - useful check to that we're pulling the correct subsets for aggregation:
        # keyby = c("YEAR", "CONTENT_AREA", "GRADE", "SchoolID")
    ]

sch_summary_cnd_2 <-
    State_A_Data_LONG[
        YEAR %in% c(2018, 2019) &
        GRADE %in% 3:8,
        .(TotalN_2 = .N,
          ProfN_2 = sum(PROFICIENCY==1L),
          GrowthN_2 = sum(!is.na(SGP_Cnd_2)),
          MGP_2 = round(mean(SGP_Cnd_2, na.rm = TRUE), 1),
          MeanScore_2 = round(mean(Z_SCORE, na.rm = TRUE), 2),
          PcntProf_2 = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          StatusZ_2 = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          GrowthZ_2 = round(mean(qnorm(SGP_Cnd_2/100), na.rm = TRUE), 3)
        ),
        keyby = c("SchoolID", "YEAR", "CONTENT_AREA")
    ]

sch_summary_cnd_3 <-
    State_A_Data_LONG[
        YEAR %in% c(2018, 2019) &
        GRADE %in% c(3, 5:6, 8),
        .(TotalN_3 = .N,
          ProfN_3 = sum(PROFICIENCY==1L),
          GrowthN_3 = sum(!is.na(SGP_Cnd_3)),
          MGP_3 = round(mean(SGP_Cnd_3, na.rm = TRUE), 1),
          MeanScore_3 = round(mean(Z_SCORE, na.rm = TRUE), 2),
          PcntProf_3 = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          StatusZ_3 = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          GrowthZ_3 = round(mean(qnorm(SGP_Cnd_3/100), na.rm = TRUE), 3)
        ),
        keyby = c("SchoolID", "YEAR", "CONTENT_AREA")
    ]

#' You may notice that there are more summary calculations than what will be
#' used (e.g., percent proficient and mean standardized scale scores). Those
#' are included for our review - so we can easily see what a z-score, of for
#' example 0.5, corresponds to in the actual percent proficient or mean SGP.
#' Here are two schools from the condition 0 table:
#'
#+ agg-cond-0-ex, echo = TRUE, purl = TRUE
sch_summary_cnd_0[SchoolID %in% c(1001, 3801)] |>
    setkey(SchoolID) |> print()

#' At some point we will want to combine the condition aggregations into a
#' single table so that we can do direct condition comparisons. We can also
#' clean up some of the extra descriptive statistics, re-order the columns or
#' anything else.
#' 
#+ agg-composite, echo = TRUE, purl = TRUE
#  Combine all condition-specific tables into one:
composite_summary <-
    sch_summary_cnd_0[
    sch_summary_cnd_1b][
    sch_summary_cnd_1c][
    sch_summary_cnd_2][
    sch_summary_cnd_3]

#  Remove extraneous aggregations:
composite_summary[,
    grep("PcntProf|Mean_", names(composite_summary), value = TRUE) :=
      NULL
]

#  School No. '1001' - changes in Growth N count
composite_summary[SchoolID == 1001,
  c("YEAR", "CONTENT_AREA",
    grep("GrowthN", names(composite_summary), value = TRUE)
  ), with = FALSE
]
#  School No. '1001' - Growth (Z-SGP) summaries
composite_summary[SchoolID == 1001,
    c("YEAR", "CONTENT_AREA",
# All relevant aggregations at once:
# sort(grep("GrowthZ|StatusZ", names(composite_summary), value = TRUE))
      grep("GrowthZ", names(composite_summary), value = TRUE) # just z-growth
    ), with = FALSE
]
#  School No. '1001' - Status (Z-proficient %) summaries
composite_summary[SchoolID == 1001,
    c("YEAR", "CONTENT_AREA",
      grep("StatusZ", names(composite_summary), value = TRUE)
    ), with = FALSE
]


#' Another way to combine all the conditions is to create a long summary table
#' with the same summary variable names for each condition and an added column
#' indicating the condition they belong to.
#' 
#' Because creating summary tables for all growth conditions only requires
#' changing the data records selected (year, grade and content areas as defined
#' for each condition) and the growth variable specified in the aggregation
#' code above, and we are going to be doing this numerous times, we will create
#' a custom function to create these tables, rather than copying the code for
#' each use case. This will also help in the demographic level summaries, which
#' require the addition of the variable of interest into the `keyby` argument
#' of the `data.table` aggregation.
#' 
#+ agrGr8r, echo = TRUE, purl = TRUE
schoolAggrGator =
  function(
    data_table,
    growth.var,
    groups = c("SchoolID", "YEAR", "CONTENT_AREA")
  ) {
    aggr.names <-
      c("TotalN", "ProfN", "GrowthN", "MGP",
        "GrowthZ", "MeanScore", "PcntProf", "StatusZ")
    frmla <-
      paste0(
        paste(groups %w/o% "CONTENT_AREA", collapse = " + "),
        " ~ CONTENT_AREA"
      ) |> as.formula()
    data_table[,
      # the list of summaries can be reduced/increased/amended as needed:
      .(TotalN = .N,
        ProfN = sum(PROFICIENCY==1L),
        GrowthN = sum(!is.na(get(growth.var))),
        MGP = round(mean(get(growth.var), na.rm = TRUE), 1),
        GrowthZ = round(mean(qnorm(get(growth.var)/100), na.rm = TRUE), 3),
        MeanScore = round(mean(Z_SCORE, na.rm = TRUE), 2),
        PcntProf = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
        StatusZ = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3)
      ),
      keyby = groups
    ] |>
      dcast(
        formula = frmla,
        sep = "..",
        value.var = aggr.names
      ) |>
        setnames(
          sapply(
            c(groups %w/o% "CONTENT_AREA",
              paste(
                rep(c("ELA", "Math"), length(aggr.names)),
                rep(aggr.names, each = 2),
                sep = "_"
            )),
            \(f) {
              tmp.name <-
                strsplit(f, "[.][.]")[[1]] |>
                  rev() |> paste(collapse = "_")
              gsub("MATHEMATICS", "Math", tmp.name)
            }
          ) |> unlist()
        )
  }

#+ agg-sgp-conds-all-stdnt, echo = FALSE, purl = TRUE
school_aggregation_all_students <-
    rbindlist(
      list(
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0"
        )[, Condition := "0"],
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% c(3, 5:6, 8)],
          growth.var = "SGP_Cnd_1b"
        )[, Condition := "1b"],
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[
              YEAR %in% c(2018, 2019) &
              (CONTENT_AREA == "ELA" & GRADE %in% c(3, 5, 7) |
              CONTENT_AREA == "MATHEMATICS" & GRADE %in% c(4, 6, 8))
            ],
          growth.var = "SGP_Cnd_1c"
        )[, Condition := "1c"],
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8],
          growth.var = "SGP_Cnd_2"
        )[, Condition := "2"],
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% c(3, 5:6, 8)],
          growth.var = "SGP_Cnd_3"
        )[, Condition := "3"]
      )
    ) |>
      setcolorder(c("Condition", "YEAR", "SchoolID"))


#' We can look at these tables in a number of ways to make sure we are getting
#' what is expected.  A simply cross-tab by year shows that many schools do not
#' get a summary in condition 1c given the testing pattern:
#' 
#+ agg-all-inspect, echo = TRUE, purl = TRUE
table(school_aggregation_all_students[, .(Condition, YEAR), is.na(Math_GrowthZ)])

fwrite(school_aggregation_all_students,
    file = "Data/School_Summaries/School_Condition_AllGrowth_AllStudents_State_A_AllYears_AVI.csv"
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
#+ agg-improve, echo = TRUE, purl = TRUE
 sch_summary_cnd_1a <-
    State_A_Data_LONG[
        GRADE %in% c(5, 8),
        .(TotalN = .N,
          MeanScore = round(mean(Z_SCORE, na.rm = TRUE), 2),
          StatusZ = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3)
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
    shift_variable = c("TotalN", "MeanScore", "StatusZ"),
    shift_amount = 1L
)

#  Subset the data for the two focus years:
sch_summary_cnd_1a <-
    sch_summary_cnd_1a[YEAR %in% c(2018, 2019)]

#  Calculate changes (current year minus 1 year lag)
sch_summary_cnd_1a[,
    TotalN_Change := TotalN - TotalN_LAG_1
][,
    MeanScore_Change := MeanScore - MeanScore_LAG_1
][,
    StatusZ_Change := StatusZ - StatusZ_LAG_1
]

#' Here is our example school's improvement numbers
sch_summary_cnd_1a[SchoolID == 1001,
    c(key(sch_summary_cnd_1a)[-1],
      grep("Change", names(sch_summary_cnd_1a), value = TRUE)
    ), with = FALSE
]

#' Per the specifications doc, here is how we could reshape this summary table
#' and output by year.
#'
#+ agg-improve-format, echo = TRUE, purl = TRUE
#  Reshape by subject
sch_summary_cnd_1a <-
    dcast(
        data = sch_summary_cnd_1a,
        formula = SchoolID + YEAR + GRADE ~ CONTENT_AREA,
        sep = "..",
        value.var = names(sch_summary_cnd_1a) %w/o% key(sch_summary_cnd_1a)
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

fwrite(sch_summary_cnd_1a[YEAR == 2018][, YEAR := NULL],
    file = "Data/School_Summaries/School_Condition_1a_State_A_2018_AVI.csv"
)
fwrite(sch_summary_cnd_1a[YEAR == 2019][, YEAR := NULL],
    file = "Data/School_Summaries/School_Condition_1a_State_A_2019_AVI.csv"
)

#' One thought before leaving the "school improvement" indicator section is
#' that the issue of "regression to the mean" should be considered. There is
#' a strong relationship between current/prior scores and change scores that
#' may require a correction:
#'
#+ agg-rtm, echo = TRUE, purl = TRUE
cor(
  sch_summary_cnd_1a[, Math_MeanScore, Math_MeanScore_Change],
  use = 'na.or.complete'
) |> round(3)

cor(
  sch_summary_cnd_1a[, Math_MeanScore_LAG_1, Math_MeanScore_Change],
  use = 'na.or.complete'
) |> round(3)

cor(
  sch_summary_cnd_1a[, ELA_MeanScore_LAG_1, ELA_MeanScore_Change],
  use = 'na.or.complete'
) |> round(3)


#' ##  School level aggregations by demographics
#'
#' Adding in the demographic variables is a simple addition of the variable of
#' interest into the `keyby` argument of the `data.table` aggregation.
#' 
#' Our original "base" condition table can be reproduced now with this call to
#' our function:
#' 
#' An example of our function used for (dis)aggregation by student demographics
#' (Economic Disadvantage) for condition 0 results:
#' 
#+ agrGr8r-demog-ex, echo = TRUE, purl = TRUE, eval = FALSE
schoolAggrGator(
    data_table =
      State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
    growth.var = "SGP_Cnd_0",
    groups = c("SchoolID", "YEAR", "CONTENT_AREA", "EconDis")
)

#' In order to do all the demographic summaries at once, we can combine calls
#' to the function (rather than creating separate tables and THEN combining)
#' using the `rbindlist` function from `data.table`.
#' Note that the demographic subgroup indicator is changed (from the demographic
#' variable name) to the generic "Group" for each individual table.:
#' 
#+ agrGr8r-combo1, echo = TRUE, purl = TRUE
demog_cond_0 <-
    rbindlist(
      list(
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("SchoolID", "YEAR", "CONTENT_AREA", "Race")
        ) |> setnames("Race", "Group"),
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("SchoolID", "YEAR", "CONTENT_AREA", "EconDis")
        ) |> setnames("EconDis", "Group"),
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("SchoolID", "YEAR", "CONTENT_AREA", "EL")
        ) |> setnames("EL", "Group"),
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("SchoolID", "YEAR", "CONTENT_AREA", "SWD")
        ) |> setnames("SWD", "Group")
      )
    )

#' Here a subset of the output from the example school:
demog_cond_0[
  SchoolID == 1001 & YEAR == 2019
][,
  grep("YEAR|SchoolID|MGP|MeanScore", names(demog_cond_0), value = TRUE) := NULL
][]

#' Another example of how to combine the aggregations uses calls to the function
#' via `lapply` over the demographic variables and piping the resulting list to
#' `rbindlist`.
#'
#+ agrGr8r-combo2, echo = TRUE, purl = TRUE, eval = FALSE
demog_cond0 <-
  lapply(
    c("Race", "EconDis", "EL", "SWD"),
    \(f) {
      schoolAggrGator(
        data_table =
          State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
        growth.var = "SGP_Cnd_0",
        groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
      )[, Condition := "0"] |> setnames(f, "Group")
    }
  ) |> rbindlist()


#' Either of the demographic aggregation and combination code chunks above can
#' be run for each of the `SGP_Cnd*` growth fields. At that point, we can then
#' combine those objects in a wide format (similar to what was done for the
#' `composite_summary` object - this would require re-naming the aggregate
#' variables), stacked into a long format (with an added "Condition" variable
#' for each table - probably what I would do) or written to separate .csv files
#' as described in the specification doc.
#'
#' The example below shows how to create a stacked long format version with all
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
                State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
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
                State_A_Data_LONG[
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
                State_A_Data_LONG[
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
                State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
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
                State_A_Data_LONG[
                  YEAR %in% c(2018, 2019) & GRADE %in% c(3, 5:6, 8), ],
              growth.var = "SGP_Cnd_3",
              groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
            )[, Condition := "3"] |> setnames(f, "Group")
          }
        ) |> rbindlist()
      )
    ) |>
      setcolorder("Condition")

fwrite(school_aggregation_student_demogs,
    file = "Data/School_Summaries/School_Condition_AllGrowth_Demographics_State_A_AllYears_AVI.csv"
)

#' ##  A single file with all aggregations
#'
#' I may be missing something about why all the various .csv files are needed
#' for this step, but if we wanted to save all the conditions and summaries as
#' a single file, we could do something like this (and then save as a single
#' .csv using `fwrite` or similar):

#+ res-agr-save-all, echo = TRUE, purl = FALSE
school_aggregation_all_students[, Group := "All Students"]
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
    file = "Data/School_Summaries/School_Condition_AllGrowth_Demographics_State_A_AllYears_AVI.csv"
)

#' ##  Summary and notes
#'
#' * A standardized scale score variable is added (scaled by unique grade,
#'   content area and annual assessment).
#' * A binary indicator variable for proficiency status is added.
#' * Methods for using the `data.table` package for calculating school level
#'   aggregations for all students and by demographic subgroups were outlined
#'   and discussed with examples.