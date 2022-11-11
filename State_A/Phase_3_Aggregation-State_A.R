#+ res-agr, include = FALSE, echo = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####     2018 and 2019 Growth and Achievement Aggregations for State A     ####
####                                                                       ####
###############################################################################
options(datatable.print.class = FALSE)
options(datatable.print.rownames = FALSE)
options(datatable.print.keys = FALSE)

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
#' ##  Condition specific summary tables
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
          Mean_Score = round(mean(Z_SCORE, na.rm = TRUE), 2),
          Pcnt_Prof = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          Z_Status = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          Z_Growth = round(mean(qnorm(SGP_Cnd_0/100), na.rm = TRUE), 3)
        ),
        keyby = c("YEAR", "CONTENT_AREA", "SchoolID")
    ]

#+ agg-sim-conds, echo = FALSE, purl = TRUE
sch_summary_cnd_1b <-
    State_A_Data_LONG[
        YEAR %in% c(2018, 2019) &
        GRADE %in% c(3, 5:6, 8),
        .(TotalN_1b = .N,
          ProfN_1b = sum(PROFICIENCY==1L),
          GrowthN_1b = sum(!is.na(SGP_Cnd_1b)),
          MGP_1b = round(mean(SGP_Cnd_1b, na.rm = TRUE), 1),
          Mean_Score_1b = round(mean(Z_SCORE, na.rm = TRUE), 2),
          Pcnt_Prof_1b = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          Z_Status_1b = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          Z_Growth_1b = round(mean(qnorm(SGP_Cnd_1b/100), na.rm = TRUE), 3)
          # T_Growth = round(mean(qt(SGP_Cnd_1b/100, df = .N-1), na.rm = TRUE), 3), # t distribution vs normal
        ),
        keyby = c("YEAR", "CONTENT_AREA", "SchoolID")
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
          Mean_Score_1c = round(mean(Z_SCORE, na.rm = TRUE), 2),
          Pcnt_Prof_1c = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          Z_Status_1c = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          Z_Growth_1c = round(mean(qnorm(SGP_Cnd_1c/100), na.rm = TRUE), 3)
        ),
        keyby = c("YEAR", "CONTENT_AREA", "SchoolID")
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
          Mean_Score_2 = round(mean(Z_SCORE, na.rm = TRUE), 2),
          Pcnt_Prof_2 = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          Z_Status_2 = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          Z_Growth_2 = round(mean(qnorm(SGP_Cnd_2/100), na.rm = TRUE), 3)
        ),
        keyby = c("YEAR", "CONTENT_AREA", "SchoolID")
    ]

sch_summary_cnd_3 <-
    State_A_Data_LONG[
        YEAR %in% c(2018, 2019) &
        GRADE %in% c(3, 5:6, 8),
        .(TotalN_3 = .N,
          ProfN_3 = sum(PROFICIENCY==1L),
          GrowthN_3 = sum(!is.na(SGP_Cnd_3)),
          MGP_3 = round(mean(SGP_Cnd_3, na.rm = TRUE), 1),
          Mean_Score_3 = round(mean(Z_SCORE, na.rm = TRUE), 2),
          Pcnt_Prof_3 = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
          Z_Status_3 = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
          Z_Growth_3 = round(mean(qnorm(SGP_Cnd_3/100), na.rm = TRUE), 3)
        ),
        keyby = c("YEAR", "CONTENT_AREA", "SchoolID")
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

#' At some point we will probably want to combine the condition aggregations
#' into a single table so that we can do direct condition comparisons. We can
#' also clean up some of the extra descriptive statistics, re-order the columns
#' or anything else.
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
    grep("Pcnt_Prof|Mean_", names(composite_summary), value = TRUE) :=
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
# sort(grep("Z_Growth|Z_Status", names(composite_summary), value = TRUE))
      grep("Z_Growth", names(composite_summary), value = TRUE) # just z-growth
    ), with = FALSE
]
#  School No. '1001' - Status (Z-proficient %) summaries
composite_summary[SchoolID == 1001,
    c("YEAR", "CONTENT_AREA",
      grep("Z_Status", names(composite_summary), value = TRUE)
    ), with = FALSE
]

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
          Mean_Score = round(mean(Z_SCORE, na.rm = TRUE), 2),
          Z_Status = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3)
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
    shift_variable = c("TotalN", "Mean_Score", "Z_Status"),
    shift_amount = 1L
)

#  Subset the data for the two focus years:
sch_summary_cnd_1a <-
    sch_summary_cnd_1a[YEAR %in% c(2018, 2019)]

#  Calculate changes (current year minus 1 year lag)
sch_summary_cnd_1a[,
    TotalN_Change := TotalN - TotalN_LAG_1
][,
    Mean_Score_Change := Mean_Score - Mean_Score_LAG_1
][,
    Z_Status_Change := Z_Status - Z_Status_LAG_1
]

#' Here is our example school's improvement numbers
sch_summary_cnd_1a[SchoolID == 1001,
    c(key(sch_summary_cnd_1a)[-1],
      "TotalN_Change", "Mean_Score_Change", "Z_Status_Change"
    ), with = FALSE
]

#' ##  School level aggregations by demographics
#'
#' Adding in the demographic variables is a simple addition of the variable of
#' interest into the `keyby` argument of the `data.table` aggregation. Since we
#' are going to be doing this numerous times, it might be smart to create a
#' custom function to create these tables, rather than copying the code for
#' each use case.
#' 
#+ agrGr8r, echo = TRUE, purl = TRUE
schoolAggrGator =
  function(
    data_table,
    growth.var,
    groups = c("YEAR", "CONTENT_AREA", "SchoolID")
  ) {
    data_table[,
      # the list of summaries can be reduced/increased/amended as needed:
      .(TotalN = .N,
        ProfN = sum(PROFICIENCY==1L),
        GrowthN = sum(!is.na(get(growth.var))),
        MGP = round(mean(get(growth.var), na.rm = TRUE), 1),
        Mean_Score = round(mean(Z_SCORE, na.rm = TRUE), 2),
        # Pcnt_Prof = round(mean(PROFICIENCY, na.rm = TRUE), 3)*100,
        Z_Status = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3),
        Z_Growth = round(mean(qnorm(get(growth.var)/100), na.rm = TRUE), 3)
      ),
      keyby = groups
    ][]
  }

#' Our original "base" condition table can be reproduced now with this call to
#' our function:
#' 
#+ agrGr8r-ex1, echo = TRUE, purl = TRUE, eval = FALSE
schoolAggrGator(
    data_table =
      State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
    growth.var = "SGP_Cnd_0",
)

#' Our function used for demographics (Economic Disadvantage):
#' 
#+ agrGr8r-ex2, echo = TRUE, purl = TRUE, eval = FALSE
schoolAggrGator(
    data_table =
      State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
    growth.var = "SGP_Cnd_0",
    groups = c("YEAR", "CONTENT_AREA", "SchoolID", "EconDis")
)

#' In order to do all the demographic summaries at once, we can combine calls
#' to the function (rather than creating separate tables and THEN combining):
#' 
#+ agrGr8r-combo1, echo = TRUE, purl = TRUE
demog_cond_0 <-
    rbindlist(
      list(
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("YEAR", "CONTENT_AREA", "SchoolID", "Race")
        ) |> setnames("Race", "Group"),
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("YEAR", "CONTENT_AREA", "SchoolID", "EconDis")
        ) |> setnames("EconDis", "Group"),
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("YEAR", "CONTENT_AREA", "SchoolID", "EL")
        ) |> setnames("EL", "Group"),
        schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8,],
          growth.var = "SGP_Cnd_0",
          groups = c("YEAR", "CONTENT_AREA", "SchoolID", "SWD")
        ) |> setnames("SWD", "Group")
      )
    )

#' Here a subset of the output from the example school:
demog_cond_0[
  SchoolID == 1001 & YEAR == 2019
][,
  c("YEAR", "SchoolID", "MGP", "Mean_Score") := NULL
][]

#' Another example of how to combine the aggregations along with output from a
#' different school:
#' 
#+ agrGr8r-combo2, echo = TRUE, purl = TRUE
demog_cond0 <-
  lapply(
    c("Race", "EconDis", "EL", "SWD"),
    \(f) {
      schoolAggrGator(
          data_table =
            State_A_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8, ],
          growth.var = "SGP_Cnd_0",
          groups = c("YEAR", "CONTENT_AREA", "SchoolID", f)
        ) |> setnames(f, "Group")
    }
  ) |> rbindlist()

demog_cond0[
  SchoolID == 3801 & YEAR == 2019
][,
  c("YEAR", "SchoolID", "MGP", "Mean_Score") := NULL
][]

#' Either of the demographic aggregation and combination code chunks above can
#' be run for each of the `SGP_Cnd*` growth fields. At that point, we can then
#' combine those objects in a wide format (similar to what was done for the
#' `composite_summary` object - this would require re-naming the aggregate
#' variables), stacked into a long format (with an added "Condition" variable
#' for each table - probably what I would do) or written to separate .csv files
#' as described in the specification doc.