#####
###   Phase 4: Calculate Comparison Metrics
#####

#  Create a subdirectory for accountability ratings
if (!dir.exists("./Data/Phase_4-Comparison_Metrics/Plots"))
     dir.create("./Data/Phase_4-Comparison_Metrics/Plots", recursive = TRUE)


##  Required packages and utility function
require(data.table)

`completeDT` =
    function(DT, cols, defs = NULL) {
        mDT = do.call(data.table::CJ, c(DT[, ..cols], list(unique = TRUE)))
        res = DT[mDT, on = names(mDT)]
        if (length(defs)) {
            res[,
            names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs),
            .SDcols = names(defs)
            ]
        }
        res[]
    }

##  Load Student Growth Data
if (!exists("Georgia_Data_LONG")) {
  source("../functions/freadZIP.R")
  Georgia_Data_LONG <-
    freadZIP(
      "Data/Student_Growth/Student_LongTestData_Georgia_2016-2019_AVI.csv.zip"
    )
}

#' ##  Additional variables for aggregated results
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
]

##  Get complete SchoolID lists
schoolIDs.18 <-
  unique(Georgia_Data_LONG[YEAR == 2018 & !is.na(SCALE_SCORE), SchoolID])
schoolIDs.19 <-
  unique(Georgia_Data_LONG[YEAR == 2019 & !is.na(SCALE_SCORE), SchoolID])


#  Schools and Student Groups with Summative Ratings
# 1. In the School-level Accountability Ratings file for the given
#    condition, state and year, identify the records with Group = "All".
#    Calculate the following:
#   a. NumSchools_All = total number of schools with Group = "All"
#   b. NumRatings_All = total number of schools with Group = "All" AND
#      SumScore is not blank
#   c. If NumSchools_All is not zero, then
#      i. PctRatings_All = NumRatings_All / NumSchools_All
#     ii. Otherwise, PctRatings_All = <blank>


ratings_percentages <- data.table()

for (cond in c("0", "1a", "1b", "1c", "2", "3", "4")) {
  for (yr in 2018:2019) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    for (min.n in c(10, 30, 50)) {
      acct_ratings <-
        fread(
          file =
            paste0(
                "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_",
                cnd.nm, "_Min", min.n, "_Georgia_", yr, "_AVI.csv"
            )
        )
      excluded_schools <-
        data.table(
          SchoolID =
            if (yr == 2018) {
              schoolIDs.18[!schoolIDs.18 %in% unique(acct_ratings[["SchoolID"]])]
            } else {
              schoolIDs.19[!schoolIDs.19 %in% unique(acct_ratings[["SchoolID"]])]
            },
          Group = "All"
        )
      acct_ratings_ext <-
        rbindlist(
            list(
            acct_ratings,
            excluded_schools
            ),
            fill = TRUE
        )
      acct_ratings_ext <-
        completeDT(acct_ratings_ext, cols = c("SchoolID", "Group"))

      tmp_acct_rates <-
        acct_ratings_ext[,
          .(NumSchools = .N,
            NumRatings = sum(!is.na(SumScore)),
            PctRatings = round((sum(!is.na(SumScore))) / .N, 3)
          ),
            keyby = "Group"
          ][,
              Sampling := cnd.nm
          ][,
              Min_N := min.n
              # `Study Condition` :=
              #     paste0("Sampling = ", cnd.nm, ", Minimum N = ", min.n)
          ][,
              Year := yr
          ] |>
            data.table::dcast(
              Sampling + Min_N + Year ~ Group,
              value.var = "PctRatings"
            )

      ratings_percentages <-
        rbindlist(
          list(
              ratings_percentages,
              tmp_acct_rates
          ),
          fill = TRUE
        )
    }
  }
}

setcolorder(
    ratings_percentages,
    c("Sampling", "Min_N", "Year", "All", "EconDis", "EL", "SWD",
      "Asian", "Black", "Hispanic", "Multiracial", "White") # , "Native American"
)

setnames(
    ratings_percentages,
    names(ratings_percentages)[-(1:3)],
    paste0("PctRatings_", names(ratings_percentages)[-(1:3)])
)


##  Students with No Growth Measure

# 4. Compute the following summary statistics for the records in the
#    Student-level Missing SGP file for the given condition, state and year:
#   a. Frequency and percentage distributions of the Race variable
#      (i.e., N_Missing_Black, %_Missing_Black,
#      N_Missing_Hispanic, %_Missing_Hispanic, etc.)
#   b. Frequency and percentage distributions of the EL, SWD, and EconDis variables
#      (i.e., N_Missing_EL, %_Missing_EL, N_Missing_SWD, %_Missing_SWD,
#      N_Missing_EconDis, and %_Missing_EconDis)
#   c. Frequency and percentage distributions of the Grade x Content_Area variables
#      (i.e., N_Missing_G3_ELA, %_Missing_G3_ELA,
#      N_Missing_G4_ELA, %_Missing_G4_ELA, etc.)
#   d. Average scale score by grade and content area
#      (i.e., AvgSS_Missing_G3_ELA, AvgSS_Missing_G4_ELA, etc.)
#   e. Frequency and percentage distributions of the Achievement_Level variable
#      (i.e., N_Missing_PL1, N_Missing_PL2, etc.)

setkey(Georgia_Data_LONG, CONTENT_AREA, GRADE)

freq_pct_race <-
freq_pct_misc <-
freq_pct_gdsubj <-
freq_pct_achlev <- data.table()

for (cond in c("0", "1b", "1c", "2", "3", "4")) { # , "1a"
  grade_subject_lookup <-
    data.table(
        Georgia_Data_LONG[
            !is.na(get(paste0("SGP_Cnd_", cond))),
            .(CONTENT_AREA, GRADE)
        ] |> unique() |> setkey(CONTENT_AREA, GRADE)
    )
  for (yr in 2018:2019) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    tmp_missing <-
      Georgia_Data_LONG[grade_subject_lookup][
        YEAR == yr &
        is.na(get(paste0("SGP_Cnd_", cond))),
      ][,
        GRADE := as.character(GRADE)
      ]

    # 4.a
    tmp_fp_race <-
      data.table(
        table(tmp_missing[, Race], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 3)
      ][,
          Sampling := cnd.nm
      ][,
      #     Min_N := min.n
      # ][,
      #   `Study Condition` := paste0("Sampling = ", cnd.nm)
      # ][,
        Year := yr
      ] |>
        data.table::dcast(
            Sampling + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Missing_"
        )
    # 4.b
    tmp_fp_misc <- rbindlist(list(
      data.table(
        table(tmp_missing[, EconDis], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 3)
      ][,
          Sampling := cnd.nm
      ][,
        Year := yr
      ],
      data.table(
        table(tmp_missing[, SWD], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 3)
      ][,
          Sampling := cnd.nm
      ][,
        Year := yr
      ],
      data.table(
        table(tmp_missing[, EL], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 3)
      ][,
          Sampling := cnd.nm
      ][,
        Year := yr
      ]
    ))[
        grep(": Yes", V1)
    ][,
        V1 := gsub(": Yes", "", V1)
    ] |>
        data.table::dcast(
            Sampling + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Missing_"
        )
    # 4.c & 4.d
    tmp_fp_gdsubj <-
      data.table(
        table(tmp_missing[, GRADE, CONTENT_AREA], exclude = NULL),
        key = c("GRADE", "CONTENT_AREA")
      )[
        data.table(
          round(prop.table(
            table(tmp_missing[, GRADE, CONTENT_AREA], exclude = NULL), 1), 3),
          key = c("GRADE", "CONTENT_AREA")
        )
      ][ # 4.d
        tmp_missing[,
          .(AvgSS = round(mean(SCALE_SCORE, na.rm = TRUE), 1)),
          keyby = c("GRADE", "CONTENT_AREA")
        ]
      ][,
          Sampling := cnd.nm
      ][,
        Year := yr
      ][,
        CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
      ][,
        GRADE := paste0("Missing_G", GRADE)
      ] |>
        setnames("i.N", "%") |>
          data.table::dcast(
            Sampling + Year ~ GRADE + CONTENT_AREA,
            value.var = c("N", "%", "AvgSS")
          )
    # 4.e
    tmp_fp_achlev <-
      data.table(
        table(tmp_missing[, ACHIEVEMENT_LEVEL], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 3)
      ][,
          Sampling := cnd.nm
      ][, Year := yr
      ][, V1 := gsub(" Learner", "", V1)
      ][, V1 := gsub("Beginning", "PL1", V1)
      ][, V1 := gsub("Developing", "PL2", V1)
      ][, V1 := gsub("Proficient", "PL3", V1)
      ][, V1 := gsub("Distinguished", "PL4", V1)
      ] |>
        data.table::dcast(
            Sampling + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Missing_"
        )
    freq_pct_race <- rbindlist(list(tmp_fp_race, freq_pct_race), fill = TRUE)
    freq_pct_misc <- rbindlist(list(tmp_fp_misc, freq_pct_misc), fill = TRUE)
    freq_pct_gdsubj <- rbindlist(list(tmp_fp_gdsubj, freq_pct_gdsubj), fill = TRUE)
    freq_pct_achlev <- rbindlist(list(tmp_fp_achlev, freq_pct_achlev), fill = TRUE)
  }
}

freq_pct_race |>
  setcolorder(
    c(1:2,
      lapply(
        gsub("N_Missing_|%_Missing_", "", names(freq_pct_race)[-(1:3)]) |> unique(),
        \(f) grep(f, names(freq_pct_race))
        ) |> unlist()
    )
  ) |>
    setkeyv(c("Sampling",  "Year"))

freq_pct_misc |>
  setcolorder(
    c(1:2,
      unlist(lapply(c("EconDis", "SWD", "EL"), \(f) grep(f, names(freq_pct_misc)))))
  ) |>
    setkeyv(c("Sampling",  "Year"))

freq_pct_gdsubj |>
  setcolorder(
    c(1:2,
      lapply(
        gsub("N_Missing_|%_Missing_|AvgSS_Missing_", "",
            names(freq_pct_gdsubj)[-(1:3)]) |> unique(),
        \(f) grep(f, names(freq_pct_gdsubj))
        ) |> unlist()
    )
  ) |>
    setkeyv(c("Sampling",  "Year"))

freq_pct_achlev |>
  setcolorder(
    c(1:2,
      unlist(lapply(1:4, \(f) grep(paste0("PL", f), names(freq_pct_achlev)))))
  ) |>
    setkeyv(c("Sampling",  "Year"))

missing_freq_pct_table <-
  freq_pct_race[freq_pct_misc][freq_pct_gdsubj][freq_pct_achlev]


###   5.a - 5.e "Full" student frequencies and percentages
setkey(Georgia_Data_LONG, CONTENT_AREA, GRADE)

freq_pct_race <-
freq_pct_misc <-
freq_pct_gdsubj <-
freq_pct_achlev <- data.table()

for (cond in c("0", "1b", "1c", "2", "3", "4")) { # , "1a"
  grade_subject_lookup <-
    data.table(
        Georgia_Data_LONG[
            !is.na(get(paste0("SGP_Cnd_", cond))),# &
            # GRADE != 3,
            .(CONTENT_AREA, GRADE)
        ] |> unique() |> setkey(CONTENT_AREA, GRADE)
    )
  for (yr in 2018:2019) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    tmp_full <-
      Georgia_Data_LONG[grade_subject_lookup][
        YEAR == yr
      ][,
        GRADE := as.character(GRADE)
      ]

    # 5.a
    tmp_fp_race <-
      data.table(
        table(tmp_full[, Race], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 3)
      ][, Sampling := cnd.nm
      ][, Year := yr
      ] |>
        data.table::dcast(
            Sampling + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Full_"
        )
    # 5.b
    tmp_fp_misc <- rbindlist(list(
      data.table(
        table(tmp_full[, EconDis], exclude = NULL)
      )[, `%` := round(prop.table(N), 3)
      ][, Sampling := cnd.nm
      ][, Year := yr
      ],
      data.table(
        table(tmp_full[, SWD], exclude = NULL)
      )[, `%` := round(prop.table(N), 3)
      ][, Sampling := cnd.nm
      ][, Year := yr
      ],
      data.table(
        table(tmp_full[, EL], exclude = NULL)
      )[, `%` := round(prop.table(N), 3)
      ][, Sampling := cnd.nm
      ][, Year := yr
      ]
    ))[
        grep(": Yes", V1)
    ][,
        V1 := gsub(": Yes", "", V1)
    ] |>
        data.table::dcast(
            Sampling + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Full_"
        )
    # 5.c & 5.d
    tmp_fp_gdsubj <-
      data.table(
        table(tmp_full[, GRADE, CONTENT_AREA], exclude = NULL),
        key = c("GRADE", "CONTENT_AREA")
      )[
        data.table(
          round(prop.table(
            table(tmp_full[, GRADE, CONTENT_AREA], exclude = NULL), 1), 3),
          key = c("GRADE", "CONTENT_AREA")
        )
      ][ # 5.d
        tmp_full[,
          .(AvgSS = round(mean(SCALE_SCORE, na.rm = TRUE), 1)),
          keyby = c("GRADE", "CONTENT_AREA")
        ]
      ][, Sampling := cnd.nm
      ][, Year := yr
      ][, CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
      ][, GRADE := paste0("Full_G", GRADE)
      ] |>
        setnames("i.N", "%") |>
          data.table::dcast(
            Sampling + Year ~ GRADE + CONTENT_AREA,
            value.var = c("N", "%", "AvgSS")
          )
    # 5.e
    tmp_fp_achlev <-
      data.table(
        table(tmp_full[, ACHIEVEMENT_LEVEL], exclude = NULL)
      )[, `%` := round(prop.table(N), 3)
      ][, Sampling := cnd.nm
      ][, Year := yr
      ][, V1 := gsub(" Learner", "", V1)
      ][, V1 := gsub("Beginning", "PL1", V1)
      ][, V1 := gsub("Developing", "PL2", V1)
      ][, V1 := gsub("Proficient", "PL3", V1)
      ][, V1 := gsub("Distinguished", "PL4", V1)
      ] |>
        data.table::dcast(
            Sampling + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Full_"
        )

    freq_pct_race <- rbindlist(list(freq_pct_race, tmp_fp_race), fill = TRUE)
    freq_pct_misc <- rbindlist(list(freq_pct_misc, tmp_fp_misc), fill = TRUE)
    freq_pct_gdsubj <- rbindlist(list(freq_pct_gdsubj, tmp_fp_gdsubj), fill = TRUE)
    freq_pct_achlev <- rbindlist(list(freq_pct_achlev, tmp_fp_achlev), fill = TRUE)
  }
}

freq_pct_race |>
  setcolorder(
    c(1:2,
      lapply(
        gsub("N_Full_|%_Full_", "", names(freq_pct_race)[-(1:3)]) |> unique(),
        \(f) grep(f, names(freq_pct_race))
      ) |> unlist()
    )
  ) |>
    setkeyv(c("Sampling",  "Year"))

freq_pct_misc |>
  setcolorder(
    c(1:2,
      unlist(lapply(c("EconDis", "SWD", "EL"), \(f) grep(f, names(freq_pct_misc)))))
  ) |>
    setkeyv(c("Sampling",  "Year"))

freq_pct_gdsubj |>
  setcolorder(
    c(1:2,
      lapply(
        gsub("N_Full_|%_Full_|AvgSS_Full_", "",
            names(freq_pct_gdsubj)[-(1:3)]) |> unique(),
        \(f) grep(f, names(freq_pct_gdsubj))
      ) |> unlist()
    )
  ) |>
    setkeyv(c("Sampling",  "Year"))

freq_pct_achlev |>
  setcolorder(
    c(1:2,
      unlist(lapply(1:4, \(f) grep(paste0("PL", f), names(freq_pct_achlev)))))
  ) |>
    setkeyv(c("Sampling",  "Year"))

full_freq_pct_table <-
  freq_pct_race[freq_pct_misc][freq_pct_gdsubj][freq_pct_achlev]


# 6.
freq_pct_table <-
  missing_freq_pct_table[full_freq_pct_table]

var.order <-
    c("_EconDis$", "_SWD$", "_EL$",
      gsub("N_Full_|%_Full_", "", names(freq_pct_race)[-(1:3)]) |> unique(),
      gsub("N_Full_|%_Full_|AvgSS_Full_", "",
            names(freq_pct_gdsubj)[-(1:3)]) |> unique(),
      "_PL1$", "_PL2$", "_PL3$", "_PL4$"
    )

freq_pct_table |>
setcolorder(
  c(1:2,
    lapply(
      var.order,
      \(f) grep(f, names(freq_pct_table))
    ) |> unlist()
  )
)


##  Schools with No Summative Ratings
#
# 7. Filter the School-level Accountability Ratings file for the given
#    condition and year to only include records for which Group = "All"
#    AND SumScore is blank. This should result in a list of schools (DSL_IDs)
#    for which summative ratings could not be computed.
# 8. Filter the Student-level Longitudinal Assessment file to only include
#    records with DSL_IDs that are in the list generated in the previous step.
# 9. Aggregated the records from the previous step by school (DSL_ID) and
#    compute the following statistics for each school:
#   a. Total_N_NoSum = total number of students (i.e., unique student IDs)
#   b. Percentage distribution of the Race variable (i.e., %_Black_NoSum,
#      %_Hispanic_NoSum, %_White_NoSum, etc.)
#   c. Percentage distribution of the EL, SWD, and EconDis variables
#      (i.e., %_EL_NoSum, %_SWD_NoSum, %_EconDis_NoSum)
#   d. Percentage of students who are proficient by content area
#      (i.e, %_Proficient_ELA_NoSum and %_Proficient_Math_NoSum)
#   e. Average scale score by grade and content area
#      (i.e., AvgSS_G3_ELA_NoSum, AvgSS_G4_ELA_NoSum, etc.)

excl_ratings <- data.table()
full_ratings <- data.table()

for (cond in c("0", "1a", "1b", "1c", "2", "3", "4")) {
  if (cond == "1a") {
    grade_subject_lookup <-
      data.table(
        CONTENT_AREA = c(rep("ELA", 2), rep("MATHEMATICS", 2)),
        GRADE = c(5, 8,  5, 8),
        key = c("CONTENT_AREA", "GRADE")
      )
  } else {
    grade_subject_lookup <-
      data.table(
          Georgia_Data_LONG[
            !is.na(get(paste0("SGP_Cnd_", cond))),
            .(CONTENT_AREA, GRADE)
          ] |> unique() |> setkey(CONTENT_AREA, GRADE)
      )
  }
  for (yr in 2018:2019) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    for (min.n in c(10, 30, 50)) {
    # 7.
      acct_ratings <-
        fread(
            file = paste0(
                "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_",
                cnd.nm, "_Min", min.n, "_Georgia_", yr, "_AVI.csv"
            )
        )
      excluded.schools <-
        if (yr == 2018) {
          c(acct_ratings[Group == "All" & is.na(SumScore), SchoolID],
            schoolIDs.18[!schoolIDs.18 %in% unique(acct_ratings[, SchoolID])]
          )
        } else {
          c(acct_ratings[Group == "All" & is.na(SumScore), SchoolID],
            schoolIDs.19[!schoolIDs.19 %in% unique(acct_ratings[["SchoolID"]])]
          )
        }
    # 8.
      tmp_excl <-
        Georgia_Data_LONG[grade_subject_lookup][
            YEAR == yr &
            SchoolID %in% excluded.schools
        ][,
            GRADE := as.character(GRADE)
        ]

    # 9.a
      excl_stu_n <-
          tmp_excl[,
              .(Total_N_NoSum = length(unique(ID)))
          ]
    # 9.b
      excl_stu_race <-
          tmp_excl[,
              .(N_NoSum = length(unique(ID))),
              "Race",
          ][,
              PCT := round((N_NoSum / unlist(excl_stu_n)), 3)
          ][, N_NoSum := NULL
          ] |>
              data.table::dcast(
                  . ~ Race,
                  value.var = "PCT"
              )
      setnames(
          excl_stu_race[, `.` := NULL],
          names(excl_stu_race),
          paste0("%_", names(excl_stu_race), "_NoSum")
      )
    # 9.c  
      excl_stu_ed <-
          tmp_excl[,
              .(`%_EconDis_NoSum` = round((sum(EconDis == "EconDis: Yes")/.N), 3)),
          ]
      excl_stu_swd <-
          tmp_excl[,
              .(`%_SWD_NoSum` = round((sum(SWD == "SWD: Yes")/.N), 3))
          ]
      excl_stu_el <-
          tmp_excl[,
              .(`%_EL_NoSum` = round((sum(EL == "EL: Yes")/.N), 3))
          ]

    # 9.d
      excl_stu_achlev <-
          tmp_excl[,
              .(PCT = round((sum(PROFICIENCY == 1)/.N), 3)),
              keyby = "CONTENT_AREA"
          ] |>
              data.table::dcast(
                  . ~ CONTENT_AREA,
                  value.var = "PCT"
              )
      setnames(
          excl_stu_achlev[, `.` := NULL],
          c("%_Proficient_ELA_NoSum", "%_Proficient_Math_NoSum")
      )

    # 9.e
      excl_stu_mss <-
          tmp_excl[,
              .(MSS = round(mean(SCALE_SCORE, na.rm = TRUE), 1)),
              keyby = c("CONTENT_AREA", "GRADE"),
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ][, GRADE := paste0("G", GRADE)
          ] |>
              data.table::dcast(
                  . ~ GRADE + CONTENT_AREA,
                  value.var = "MSS"
              )
      setnames(
          excl_stu_mss[, `.` := NULL],
          paste0("AvgSS_", names(excl_stu_mss), "_NoSum")
      )

    # 9.f
      if (cond == "1a") {
        excl_stu_msgp <- excl_stu_cr <- NULL
      } else {
        excl_stu_msgp <-
          tmp_excl[,
              .(MSGP = round(mean(get(paste0("SGP_Cnd_", cond)), na.rm = TRUE), 1)),
              keyby = c("CONTENT_AREA", "GRADE"),
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ][, GRADE := paste0("G", GRADE)
          ] |>
              data.table::dcast(
                  . ~ GRADE + CONTENT_AREA,
                  value.var = "MSGP"
              )
        setnames(
          excl_stu_msgp[, `.` := NULL],
          paste0("AvgSGP_", names(excl_stu_msgp), "_NoSum")
        )
        excl_stu_cr <-
          tmp_excl[,
              .(Calc_Rate =
                  round(sum(is.na(get(paste0("SGP_Cnd_", cond))))/.N, 3)
              ),
              keyby = "CONTENT_AREA"
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ] |>
              data.table::dcast(
                  . ~ CONTENT_AREA,
                  value.var = "Calc_Rate"
              )
        setnames(
          excl_stu_cr[, `.` := NULL],
          paste0("Missing_SGP_", names(excl_stu_cr), "_NoSum")
        )
      }
      # Combine into single table
      tmp_excl_ratings <-
        cbind(
            excl_stu_n, excl_stu_ed, excl_stu_swd, excl_stu_el, excl_stu_race,
            excl_stu_achlev, excl_stu_mss, excl_stu_msgp, excl_stu_cr
        )[,
            Sampling := cnd.nm
        ][, Min_N := min.n
        ][, Year := yr
        ][]
      excl_ratings <-
          rbindlist(
              list(excl_ratings, tmp_excl_ratings),
              fill = TRUE
          )
    } ## End `min.n` loop here
    ##  "Full" summaries don't change depending on Min N

# 10. Compute the same summary statistics for all records (i.e., unfiltered)
#     in the Student-level Longitudinal Assessment; that is: 
#   a. Total_N_Full = total number of students (i.e., unique student IDs)
#   b. Percentage distribution of the Race variable (i.e., %_Black_Full,
#      %_Hispanic_Full, %_White_Full, etc.)
#   c. Percentage distribution of the EL, SWD, and EconDis variables
#      (i.e., %_EL_Full, %_SWD_Full, %_EconDis_Full)
#   d. Percentage of students who are proficient by content area
#      (i.e, %_Proficient_ELA_Full and %_Proficient_Math_Full)
#   e. Average scale score by grade and content area
#      (i.e., AvgSS_G3_ELA_Full, AvgSS_G4_ELA_Full, etc.)

    # 10
      tmp_full <-
          Georgia_Data_LONG[grade_subject_lookup][YEAR == yr][,
              GRADE := as.character(GRADE)
          ]
    # 10.a
      full_stu_n <-
          tmp_full[,
              .(Total_N_Full = length(unique(ID)))
          ]
    # 10.b
      full_stu_race <-
          tmp_full[,
              .(N_Full = length(unique(ID))),
              "Race",
          ][, PCT := round((N_Full / unlist(full_stu_n)), 3)
          ][, N_Full := NULL
          ] |>
              data.table::dcast(
                  . ~ Race,
                  value.var = "PCT"
              )
      setnames(
          full_stu_race[, `.` := NULL],
          names(full_stu_race),
          paste0("%_", names(full_stu_race), "_Full")
      )
    # 10.c  
      full_stu_ed <-
          tmp_full[,
              .(`%_EconDis_Full` = round((sum(EconDis == "EconDis: Yes")/.N), 3)),
          ]
      full_stu_swd <-
          tmp_full[,
              .(`%_SWD_Full` = round((sum(SWD == "SWD: Yes")/.N), 3))
          ]
      full_stu_el <-
          tmp_full[,
              .(`%_EL_Full` = round((sum(EL == "EL: Yes")/.N), 3))
          ]

    # 10.d
      full_stu_achlev <-
          tmp_full[,
              .(PCT = round((sum(PROFICIENCY == 1)/.N), 3)),
              keyby = "CONTENT_AREA"
          ] |>
              data.table::dcast(
                  . ~ CONTENT_AREA,
                  value.var = "PCT"
              )
      setnames(
          full_stu_achlev[, `.` := NULL],
          c("%_Proficient_ELA_Full", "%_Proficient_Math_Full")
      )

    # 10.e
      full_stu_mss <-
          tmp_full[,
              .(MSS = round(mean(SCALE_SCORE, na.rm = TRUE), 1)),
              keyby = c("CONTENT_AREA", "GRADE"),
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ][, GRADE := paste0("G", GRADE)
          ] |>
              data.table::dcast(
                  . ~ GRADE + CONTENT_AREA,
                  value.var = "MSS"
              )
      setnames(
          full_stu_mss[, `.` := NULL],
          paste0("AvgSS_", names(full_stu_mss), "_Full")
      )

    # 10.f
      if (cond == "1a") {
        full_stu_msgp <- full_stu_cr <- NULL
      } else {
        full_stu_msgp <-
          tmp_full[,
              .(MSGP = round(mean(get(paste0("SGP_Cnd_", cond)), na.rm = TRUE), 1)),
              keyby = c("CONTENT_AREA", "GRADE"),
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ][, GRADE := paste0("G", GRADE)
          ] |>
              data.table::dcast(
                  . ~ GRADE + CONTENT_AREA,
                  value.var = "MSGP"
              )
        setnames(
          full_stu_msgp[, `.` := NULL],
          paste0("AvgSGP_", names(full_stu_msgp), "_Full")
        )
        full_stu_cr <-
          tmp_full[,
              .(Calc_Rate =
                  round(sum(is.na(get(paste0("SGP_Cnd_", cond))))/.N, 3)
              ),
              keyby = "CONTENT_AREA"
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ] |>
              data.table::dcast(
                  . ~ CONTENT_AREA,
                  value.var = "Calc_Rate"
              )
        setnames(
          full_stu_cr[, `.` := NULL],
          paste0("Missing_SGP_", names(full_stu_cr), "_Full")
        )
      }
      # Combine into single table
      tmp_full_ratings <-
        cbind(
            full_stu_n, full_stu_ed, full_stu_swd, full_stu_el, full_stu_race,
            full_stu_achlev, full_stu_mss, full_stu_msgp, full_stu_cr
        )[,
            Sampling := cnd.nm
        # ][, Min_N := min.n
        ][, Year := yr
        ][]
      full_ratings <-
          rbindlist(
              list(full_ratings, tmp_full_ratings),
              fill = TRUE
          )
    # }
  }
}

# 11.
missing_ratings_table <-
    merge(excl_ratings, full_ratings, by = c("Sampling", "Year"))

var.order <-
    c("Sampling", "Min_N", "Year", # "_EconDis$", "_SWD$", "_EL$",
      # gsub("N_Full_|%_Full_|_Full", "", names(full_ratings)) |> unique(),
      gsub("N_Full_|%_Full_|AvgSS_|AvgSGP_|_Full", "", # %_Proficient_ - only CA,
            names(full_ratings))
    ) |> unique()

missing_ratings_table |>
  setcolorder(
      lapply(
        var.order,
        \(f) grep(f, names(missing_ratings_table))
      ) |> unlist()
  )



#####
###   By School Aggregations
#####

school_level_ratings <- data.table()

for (cond in c("0", "1b", "1c", "2", "3", "4")) { # No "1a",
  grade_subject_lookup <-
    data.table(
        Georgia_Data_LONG[
          !is.na(get(paste0("SGP_Cnd_", cond))),
          .(CONTENT_AREA, GRADE)
        ] |> unique() |> setkey(CONTENT_AREA, GRADE)
    )
  for (yr in 2018:2019) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    # Get data first here
    tmp_full <-
        Georgia_Data_LONG[grade_subject_lookup][YEAR == yr][,
            GRADE := as.character(GRADE)
        ]

# 10. Compute the same summary statistics for all records (i.e., unfiltered)
  # 10.a
    full_stu_n <-
        tmp_full[,
          .(Total_N = length(unique(ID))),
          keyby = "SchoolID"
        ]
  # 10.b
    full_stu_race <-
        tmp_full[,
          .(N = length(unique(ID))),
          keyby = c("SchoolID", "Race"),
        ][full_stu_n][,
          PCT := round((N / Total_N), 3)
        ][,
          c("Total_N", "N") := NULL
        ] |>
          data.table::dcast(
            SchoolID ~ Race,
            value.var = "PCT"
          )
    setnames(
        full_stu_race,
        names(full_stu_race)[-1],
        paste0("%_", names(full_stu_race)[-1])
    )

    full_stu_ed <- # 10.c
        tmp_full[,
            .(`%_EconDis` = round((sum(EconDis == "EconDis: Yes")/.N), 3)),
            keyby = "SchoolID"
        ]
    full_stu_swd <-
        tmp_full[,
            .(`%_SWD` = round((sum(SWD == "SWD: Yes")/.N), 3)),
            keyby = "SchoolID"
        ]
    full_stu_el <-
        tmp_full[,
            .(`%_EL` = round((sum(EL == "EL: Yes")/.N), 3)),
            keyby = "SchoolID"
        ]

  # 10.d
    full_stu_achlev <-
        tmp_full[,
            .(PCT = round((sum(PROFICIENCY == 1)/.N), 3)),
            keyby = c("SchoolID", "CONTENT_AREA"),
        ] |>
            data.table::dcast(
                SchoolID ~ CONTENT_AREA,
                value.var = "PCT"
            )
    setnames(
          full_stu_achlev,
          c("SchoolID", "%_Proficient_ELA", "%_Proficient_Math")
    )

  # 10.e
     full_stu_mss <-
          tmp_full[,
              .(MSS = round(mean(SCALE_SCORE, na.rm = TRUE), 1)),
              keyby = c("SchoolID", "CONTENT_AREA", "GRADE"),
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ][,
              GRADE := paste0("G", GRADE)
          ] |>
              data.table::dcast(
                  SchoolID ~ GRADE + CONTENT_AREA,
                  value.var = "MSS"
              )
    setnames(
          full_stu_mss,
          names(full_stu_mss)[-1],
          paste0("AvgSS_", names(full_stu_mss)[-1])
    )

    # 10.f
      if (cond == "1a") {
        full_stu_msgp <- full_stu_cr <- NULL
      } else {
        full_stu_msgp <-
          tmp_full[,
              .(MSGP = round(mean(get(paste0("SGP_Cnd_", cond)), na.rm = TRUE), 1)),
              keyby = c("SchoolID", "CONTENT_AREA", "GRADE"),
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ][, GRADE := paste0("G", GRADE)
          ] |>
              data.table::dcast(
                  SchoolID ~ GRADE + CONTENT_AREA,
                  value.var = "MSGP"
              )
        setnames(
          full_stu_msgp,
          names(full_stu_msgp)[-1],
          paste0("AvgSGP_", names(full_stu_msgp)[-1])
        )
        full_stu_cr <-
          tmp_full[,
              .(Calc_Rate =
                  round(sum(is.na(get(paste0("SGP_Cnd_", cond))))/.N, 3)
              ),
              keyby = c("SchoolID", "CONTENT_AREA")
          ][,
              CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
          ] |>
              data.table::dcast(
                  SchoolID ~ CONTENT_AREA,
                  value.var = "Calc_Rate"
              )
        setnames(
          full_stu_cr,
          names(full_stu_cr)[-1],
          paste0("Missing_SGP_", names(full_stu_cr)[-1])
        )
      }

  # Combine (wide) into single table
    tmp_schlev_ratings <-
        full_stu_n[full_stu_ed][full_stu_swd][full_stu_el][full_stu_race][
            full_stu_achlev][full_stu_mss][full_stu_msgp][full_stu_cr][,
                Sampling := cnd.nm
            # ][, Min_N := min.n
            ][, Year := yr
            ][]

    ##  Add indicators for ratings inclusion/exclusion
    for (min.n in c(10, 30, 50)) {
      # 7.
      acct_ratings <-
        fread(
          file = paste0(
              "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_",
              cnd.nm, "_Min", min.n, "_Georgia_", yr, "_AVI.csv"
          )
        )
      all_sumscore <- acct_ratings[Group == "All", .(SchoolID, SumScore)]
      tmp_schlev_ratings <-
          merge(tmp_schlev_ratings, all_sumscore, by = "SchoolID", all.x = TRUE)
      setnames(
          tmp_schlev_ratings, "SumScore",
          paste0("Min_", min.n, "_SumScore")
      )
      # excluded.schools <-
      #   if (yr == 2018) {
      #     c(acct_ratings[Group == "All" & is.na(SumScore), SchoolID],
      #       schoolIDs.18[!schoolIDs.18 %in% unique(acct_ratings[, SchoolID])]
      #     )
      #   } else {
      #     c(acct_ratings[Group == "All" & is.na(SumScore), SchoolID],
      #       schoolIDs.19[!schoolIDs.19 %in% unique(acct_ratings[["SchoolID"]])]
      #     )
      #   }
      # tmp_schlev_ratings[,
      #     eval(paste0("Min_", min.n, "_Rating")) :=
      #       ifelse(SchoolID %in% excluded.schools, FALSE, TRUE)
      # ]
    }

  #  Combine (long) a) complete results with b) other conditions
    school_level_ratings <-
        rbindlist(
            list(
                school_level_ratings,
                tmp_schlev_ratings
            ),
            fill = TRUE
        )
  }
}

var.order <-
    c("Sampling", "Year",  "SchoolID", "Total_N",
      paste0("Min_", c(10, 30, 50), "_Rating"),
      paste0("%_", c("EconDis", "EL", "SWD",
         "Asian", "Black", "Hispanic",
         "Native American", "Multiracial", "White")),
      "%_Proficient_ELA", "%_Proficient_Math",
      paste0("G", c(4:8, 4:8), "_", c("ELA", "Math"))|> sort()
    ) |> unique()

school_level_ratings |>
  setcolorder(
      lapply(
        var.order,
        \(f) grep(f, names(school_level_ratings))
      ) |> unlist()
  )



focal_0_10N_2018 <-
  fread(
    file =
      "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_0_Min10_Georgia_2018_AVI.csv"
  )
focal_0_10N_2019 <-
  fread(
    file =
      "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_0_Min10_Georgia_2019_AVI.csv"
  )
focal_0_30N_2018 <-
  fread(
    file =
      "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_0_Min30_Georgia_2018_AVI.csv"
  )
focal_0_30N_2019 <-
  fread(
    file =
      "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_0_Min30_Georgia_2019_AVI.csv"
  )
focal_0_50N_2018 <-
  fread(
    file =
      "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_0_Min50_Georgia_2018_AVI.csv"
  )
focal_0_50N_2019 <-
  fread(
    file =
      "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_0_Min50_Georgia_2019_AVI.csv"
  )


# 12. Merge the records in the School-level Accountability Ratings files for the
#     specific reference and focal condition comparisons for the given state and
#     year by DSL_ID and Group. Keep only the school-level records; that is, the
#     records for which Group = “All”.
#   a. For example, for the sampling condition 0 vs. 1a comparison under
#      MinN = 10, the records to merge are from the files:
#      School_AcctRatings_Condition_0_Min10 _<State>_<FYear>_ <init>.csv and
#      School_AcctRatings_Condition_1a_Min10 _<State>_<FYear>_ <init>.csv.
# 13. Let CSI_Ref be the CSI variable for the reference condition and CSI_Foc be
#     the CSI variable for the focal condition. Compute the following values:
#   a. N_Schools = Total number of schools that are in both the reference and
#      focal condition School-level Accountability Ratings files
#   b. N_Match = Total number of schools for which CSI_Ref = CSI_Foc
#      (i.e., CSI_Ref and CSI_Foc are both 1 OR CSI_Ref and CSI_Foc are both 0)
#   c. %_Match = N_Match / N_Schools
#   d. N_FalsePos = Total number of schools for which CSI_Ref = 0 and CSI_Foc = 1  
#   e. %_FalsePos = N_FalsePos / N_Schools
#   f. N_FalseNeg = Total number of schools for which CSI_Ref = 1 and CSI_Foc = 0  
#   g. %_FalseNeg = N_FalsePos / N_Schools

cond.comp.csi <-
cond.comp.atsi <-
acad.indc.dist <-
min.n.comp.csi <-
min.n.comp.atsi <-
min.n.indc.dist <- data.table()

for (yr in 2018:2019) {
  for (cond in c("0", "1a", "1b", "1c", "2", "3", "4")) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    if (cond != "0") {
      for (min.n in c(10, 30, 50)) {
        refr_rates <-
          fread(
            file = paste0(
              "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_0",
              "_Min", min.n, "_Georgia_", yr, "_AVI.csv"
            )
          )
        focl_rates <-
          fread(
            file = paste0(
              "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_",
              cnd.nm, "_Min", min.n, "_Georgia_", yr, "_AVI.csv"
            )
          )
      # 12 a.
        tmp_data <-
            merge(
              refr_rates[Group == "All",
                .(SchoolID, CSI, ACH_Z, Other_Z, SumScore)
              ],
              focl_rates[Group == "All",
                .(SchoolID, CSI, ACH_Z, Other_Z, SumScore)
              ],
              by = "SchoolID", all = TRUE
            )
      # CSI - 13. & 14.
        n_tbl <- table(tmp_data[, CSI.x, CSI.y], exclude = NULL)
        p_tbl <- prop.table(n_tbl) |> round(3)

        cond.comp.tmp <-
          data.table(
            Comparison = paste0("Conditions 0 vs. ", cnd.nm, ", MinN = ", min.n),
            Year = yr,
            N_Schools    = sum(n_tbl),
            # N_Match      = sum(diag(n_tbl)),
            `%_Match`    = sum(diag(p_tbl)),
            # N_FalsePos   = n_tbl[2, 1],
            `%_FalsePos` = p_tbl[2, 1],
            # N_FalseNeg   = n_tbl[1, 2],
            `%_FalseNeg` = p_tbl[1, 2]
          )
        cond.comp.csi <-
          rbindlist(list(cond.comp.csi, cond.comp.tmp), fill = TRUE)

      # ATSI - 15. & 16.
        n_tbl <-
          focl_rates[Group != "All" & !is.na(ATSI),
            .(ATSI_FOC = ifelse(any(ATSI == 1), 1, 0)),
            keyby = "SchoolID"
          ][
          refr_rates[Group != "All" & !is.na(ATSI),
            .(ATSI_REF = ifelse(any(ATSI == 1), 1, 0)),
            keyby = "SchoolID"
          ]][, SchoolID := NULL] |>
            table(exclude = NULL)
        p_tbl <- prop.table(n_tbl) |> round(3)

        cond.comp.tmp <-
          data.table(
            Comparison = paste0("Conditions 0 vs. ", cnd.nm, ", MinN = ", min.n),
            Year = yr,
            N_Schools    = sum(n_tbl),
            # N_Match      = sum(diag(n_tbl)),
            `%_Match`    = sum(diag(p_tbl)),
            # N_FalsePos   = n_tbl[2, 1],
            `%_FalsePos` = p_tbl[2, 1],
            # N_FalseNeg   = n_tbl[1, 2],
            `%_FalseNeg` = p_tbl[1, 2]
          )
        cond.comp.atsi <-
          rbindlist(list(cond.comp.atsi, cond.comp.tmp), fill = TRUE)

        # 17. - 21. -- Distribution of Achievement and Other Indicator Scores
        acad.indc.tmp <-
          cbind(
            data.table(
              Comparison =
                paste0("Conditions 0 vs. ", cnd.nm, ", MinN = ", min.n),
              Year = yr
            ),
            tmp_data[,
              .(ACH_Correl = cor(ACH_Z.x, ACH_Z.y, use = "complete.obs"),
                ACH_RMSE =
                  sqrt(sum((ACH_Z.x - ACH_Z.y)^2, na.rm = TRUE) / sum(n_tbl)),
                Other_Correl = cor(Other_Z.x, Other_Z.y, use = "complete.obs"),
                Other_RMSE =
                  sqrt(sum((Other_Z.x - Other_Z.y)^2, na.rm = TRUE) / sum(n_tbl)),
                SumScore_Correl = cor(SumScore.x, SumScore.y, use = "complete.obs"),
                SumScore_RMSE =
                  sqrt(sum((SumScore.x - SumScore.y)^2, na.rm = TRUE) / sum(n_tbl))
              )
            ] |> round(3)
          )
        acad.indc.dist <-
          rbindlist(list(acad.indc.dist, acad.indc.tmp), fill = TRUE)
        
        grDevices::pdf(
          file =
            file.path(
              "./Data/Phase_4-Comparison_Metrics/Plots",
              paste0("Conditions_0_v_", cnd.nm, "__MinN_", min.n, "__", yr, ".pdf")
            ),
          width = 8, height = 11
        )
        par(mfrow = c(3, 2))
        plot(tmp_data[, .(ACH_Z.x, ACH_Z.y)],
             xlab = paste("Achievement Z - Condition 0, Min N =", min.n),
             ylab = paste("Achievement Z - Condition", cnd.nm, ", Min N =", min.n)
        )
        plot(tmp_data[, .(ACH_Z.x, (ACH_Z.x - ACH_Z.y))],
             xlab = paste("Achievement Z - Condition 0, Min N =", min.n),
             ylab = paste("Achievement Z Conditional Difference", "( 0 -", cnd.nm, ")")
        )

        plot(tmp_data[, .(Other_Z.x, Other_Z.y)],
             xlab = paste(
              ifelse(cond == "1a", "Improvement", "Growth (MSGP)"),
              "Z - Condition 0, Min N =", min.n
            ),
             ylab = paste(
              ifelse(cond == "1a", "Improvement", "Growth (MSGP)"),
              "Z - Condition", cnd.nm, ", Min N =", min.n
            )
        )
        plot(tmp_data[, .(Other_Z.x, (Other_Z.x - Other_Z.y))],
             xlab = paste(
               ifelse(cond == "1a", "Improvement", "Growth (MSGP)"),
              "Z - Condition 0, Min N =", min.n
            ),
             ylab = paste(
              ifelse(cond == "1a", "Improvement", "Growth (MSGP)"),
              "Z Conditional Difference", "( 0 -", cnd.nm, ")"
            )
        )

        plot(tmp_data[, .(SumScore.x, SumScore.y)],
             xlab = paste("Sum Score Z - Condition 0, Min N =", min.n),
             ylab = paste("Sum Score Z - Condition", cnd.nm, ", Min N =", min.n)
        )
        plot(tmp_data[, .(SumScore.x, (SumScore.x - SumScore.y))],
             xlab = paste("Sum Score Z - Condition 0, Min N =", min.n),
             ylab = paste("Sum Score Z Conditional Difference", "( 0 -", cnd.nm, ")")
        )

        grDevices::dev.off()
      }
    }

    for (min.n in c(30, 50)) {
      refr_rates <-
        fread(
          file = paste0(
            "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_",
            cnd.nm, "_Min10", "_Georgia_", yr, "_AVI.csv"
          )
        )
      focl_rates <-
        fread(
          file = paste0(
            "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_",
            cnd.nm, "_Min", min.n, "_Georgia_", yr, "_AVI.csv"
          )
        )
      # 12 b.
      tmp_data <-
          merge(
            refr_rates[Group == "All",
              .(SchoolID, CSI, ACH_Z, Other_Z, SumScore)
            ],
            focl_rates[Group == "All",
              .(SchoolID, CSI, ACH_Z, Other_Z, SumScore)
            ],
            by = "SchoolID", all = TRUE
          )    # CSI - 13. & 14.
      n_tbl <- table(tmp_data[, CSI.x, CSI.y], exclude = NULL)
      p_tbl <- prop.table(n_tbl) |> round(3)

      # 14.
      min.n.comp.tmp <-
        data.table(
          Comparison = paste0("MinN = 10 vs. MinN = ", min.n, ", Condition = ", cnd.nm),
          Year = yr,
          N_Schools    = sum(n_tbl),
          `%_Match`    = sum(diag(p_tbl)),
          `%_FalsePos` = p_tbl[2, 1],
          `%_FalseNeg` = p_tbl[1, 2]
        )
      min.n.comp.csi <-
        rbindlist(list(min.n.comp.csi, min.n.comp.tmp), fill = TRUE)

    # ATSI - 15. & 16.
      n_tbl <-
        focl_rates[Group != "All" & !is.na(ATSI),
          .(ATSI_FOC = ifelse(any(ATSI == 1), 1, 0)),
          keyby = "SchoolID"
        ][
        refr_rates[Group != "All" & !is.na(ATSI),
          .(ATSI_REF = ifelse(any(ATSI == 1), 1, 0)),
          keyby = "SchoolID"
        ]][, SchoolID := NULL] |>
          table(exclude = NULL)
      p_tbl <- prop.table(n_tbl) |> round(3)

      min.n.comp.tmp <-
        data.table( # MinN = 10 vs. MinN = 30, Condition = 0
          Comparison = paste0("MinN = 10 vs. MinN = ", min.n, ", Condition = ", cnd.nm),
          Year = yr,
          N_Schools    = sum(n_tbl),
          `%_Match`    = sum(diag(p_tbl)),
          `%_FalsePos` = p_tbl[2, 1],
          `%_FalseNeg` = p_tbl[1, 2]
        )
      min.n.comp.atsi <-
        rbindlist(list(min.n.comp.atsi, min.n.comp.tmp), fill = TRUE)

      # 17. - 21. -- Distribution of Achievement and Other Indicator Scores
      min.n.indc.tmp <-
        cbind(
          data.table(
            Comparison =
              paste0("MinN = 10 vs. MinN = ", min.n, ", Condition = ", cnd.nm),
            Year = yr
          ),
          tmp_data[,
            .(ACH_Correl = cor(ACH_Z.x, ACH_Z.y, use = "complete.obs"),
              ACH_RMSE =
                sqrt(sum((ACH_Z.x - ACH_Z.y)^2, na.rm = TRUE) / sum(n_tbl)),
              Other_Correl = cor(Other_Z.x, Other_Z.y, use = "complete.obs"),
              Other_RMSE =
                sqrt(sum((Other_Z.x - Other_Z.y)^2, na.rm = TRUE) / sum(n_tbl)),
              SumScore_Correl = cor(SumScore.x, SumScore.y, use = "complete.obs"),
              SumScore_RMSE =
                sqrt(sum((SumScore.x - SumScore.y)^2, na.rm = TRUE) / sum(n_tbl))
            )] |> round(3)
        )
      min.n.indc.dist <-
        rbindlist(list(min.n.indc.dist, min.n.indc.tmp), fill = TRUE)
    }
  }
}


#####
###   Output Tables to MS Excel
#####

require(openxlsx)

# ratings_percentages # 3
# freq_pct_table # 6
# missing_ratings_table # 11

# cond.comp.csi # 14a
# cond.comp.atsi # 16a
# acad.indc.dist # 19a
# min.n.comp.csi # 14b
# min.n.comp.atsi # 16b
# min.n.indc.dist #19b

## Styles for openxlsx
hs1 <-
  createStyle(
    halign = "CENTER", fgFill = "#0096FF", textDecoration = "Bold",
    border = "Bottom", wrapText = TRUE
  )
cond.sty <- createStyle(fgFill = "#D9D9D9")

achc.sty <- createStyle(fgFill = "#FCFEAC")
achr.sty <- createStyle(fgFill = "#FFFC6F")
othc.sty <- createStyle(fgFill = "#7FA7FF")
othr.sty <- createStyle(fgFill = "#7A69FF")
smsc.sty <- createStyle(fgFill = "#A9FA9A")
smsr.sty <- createStyle(fgFill = "#69FF69")

prob.flag <- createStyle(fontColour = "#F51505")

# med.sty <- createStyle(fgFill = "#BDD7EE")
# cnt.sty <- createStyle(fgFill = "#DDEBF7")
# udif.sty <- createStyle(fgFill = "#D8E4BC")
# adif.sty <- createStyle(fgFill = "#C4D79B")

# impct.sty <- createStyle(bgFill = "#FFFF00")

# impr.sty <- createStyle(bgFill = "#F2F2F2")
# none.sty <- createStyle(bgFill = "#D9D9D9")
# modr.sty <- createStyle(bgFill = "#BFBFBF")
# larg.sty <- createStyle(bgFill = "#A6A6A6")
# sver.sty <- createStyle(fontColour = "#F51505", bgFill = "#808080")


##    create a workbook
phase_4_wb <- createWorkbook(creator = NULL)

##    Add a worksheet for School Ratings Percentages/Proportions
addWorksheet(phase_4_wb, "Ratings Pct (Table 3)")
addStyle(phase_4_wb, "Ratings Pct (Table 3)", style = cond.sty,
         rows = 1:nrow(ratings_percentages)+1, cols = 1:3, gridExpand = TRUE
)
freezePane(phase_4_wb, "Ratings Pct (Table 3)", firstActiveRow = 2, firstActiveCol = 4)
writeData(phase_4_wb, "Ratings Pct (Table 3)", ratings_percentages, headerStyle = hs1)

# setColWidths(phase_4_wb, "Ratings Pct (Table 3)", cols = c(2,4), widths = 25)
# setColWidths(phase_4_wb, "Ratings Pct (Table 3)", cols = grep("Impact", names(tmp.table)), widths = 15)

# for (i in grep("# of SGPs", names(tmp.table)))
#   addStyle(phase_4_wb, "Ratings Pct (Table 3)", style = cnt.sty, rows = 1:nrow(tmp.table)+1, cols = i)
# for (i in grep("Median SGP", names(tmp.table)))
#   addStyle(phase_4_wb, "Ratings Pct (Table 3)", style = med.sty, rows = 1:nrow(tmp.table)+1, cols = i)
# for (i in grep("Unadjusted", names(tmp.table)))
#   addStyle(phase_4_wb, "Ratings Pct (Table 3)", style = udif.sty, rows = 1:nrow(tmp.table)+1, cols = i)
# for (i in grep(" Adjusted", names(tmp.table)))
#   addStyle(phase_4_wb, "Ratings Pct (Table 3)", style = adif.sty, rows = 1:nrow(tmp.table)+1, cols = i)

# freezePane(phase_4_wb, "Ratings Pct (Table 3)", firstActiveRow = 2, firstActiveCol = 4) # firstRow = TRUE,
# writeData(phase_4_wb, "Ratings Pct (Table 3)", ratings_percentages, headerStyle = hs1)


# impct.cols <- grep("Impact", names(tmp.table))

# conditionalFormatting(phase_4_wb, "Ratings Pct (Table 3)", cols = rtg.cor.cols, rows=1, type = "contains", rule = "Impact ALL", style = impct.sty)

# for (impct in impct.cols) {
#   conditionalFormatting(phase_4_wb, "Ratings Pct (Table 3)", cols = rtg.cor, rows = row.indx, type = "contains", rule = "Improvement", style = impr.sty)
#   conditionalFormatting(phase_4_wb, "Ratings Pct (Table 3)", cols = rtg.cor, rows = row.indx, type = "contains", rule = "Modest to None", style = none.sty)
#   conditionalFormatting(phase_4_wb, "Ratings Pct (Table 3)", cols = rtg.cor, rows = row.indx, type = "contains", rule = "Moderate", style = modr.sty)
#   conditionalFormatting(phase_4_wb, "Ratings Pct (Table 3)", cols = rtg.cor, rows = row.indx, type = "contains", rule = "Large", style = larg.sty)
#   conditionalFormatting(phase_4_wb, "Ratings Pct (Table 3)", cols = rtg.cor, rows = row.indx, type = "contains", rule = "Severe", style = sver.sty)
# }

##    Add a worksheet for Students with No Growth Measures by Study Condition (6.)
addWorksheet(phase_4_wb, "Students No Growth (Table 6)")
addStyle(phase_4_wb, "Students No Growth (Table 6)", style = cond.sty,
         rows = 1:nrow(freq_pct_table)+1, cols = 1:2, gridExpand = TRUE
)
freezePane(phase_4_wb, "Students No Growth (Table 6)", firstActiveRow = 2, firstActiveCol = 3)
writeData(phase_4_wb, "Students No Growth (Table 6)", freq_pct_table, headerStyle = hs1)

##    Add a worksheet for Schools with No Summative Ratings (11.)
addWorksheet(phase_4_wb, "Schools No Ratings (Table 11)")
addStyle(phase_4_wb, "Schools No Ratings (Table 11)", style = cond.sty,
         rows = 1:nrow(missing_ratings_table)+1, cols = 1:3, gridExpand = TRUE
)
freezePane(phase_4_wb, "Schools No Ratings (Table 11)", firstActiveRow = 2, firstActiveCol = 4)
writeData(phase_4_wb, "Schools No Ratings (Table 11)", missing_ratings_table, headerStyle = hs1)

##    Add a worksheet for Schools Identified for CSI (14.)
addWorksheet(phase_4_wb, "CSI Conditions (Table 14a)")
setColWidths(phase_4_wb, "CSI Conditions (Table 14a)", cols = 1, widths = 25)
addStyle(phase_4_wb, "CSI Conditions (Table 14a)", style = cond.sty,
         rows = 1:nrow(cond.comp.csi)+1, cols = 1:2, gridExpand = TRUE
)
freezePane(phase_4_wb, "CSI Conditions (Table 14a)", firstActiveRow = 2, firstActiveCol = 3)
writeData(phase_4_wb, "CSI Conditions (Table 14a)", cond.comp.csi, headerStyle = hs1)

addWorksheet(phase_4_wb, "CSI Min N (Table 14b)")
setColWidths(phase_4_wb, "CSI Min N (Table 14b)", cols = 1, widths = 32)
addStyle(phase_4_wb, "CSI Min N (Table 14b)", style = cond.sty,
         rows = 1:nrow(min.n.comp.csi)+1, cols = 1:2, gridExpand = TRUE
)
freezePane(phase_4_wb, "CSI Min N (Table 14b)", firstActiveRow = 2, firstActiveCol = 3)
writeData(phase_4_wb, "CSI Min N (Table 14b)", min.n.comp.csi, headerStyle = hs1)

##    Add a worksheet for Schools Identified for ATSI (16.)
addWorksheet(phase_4_wb, "ATSI Conditions (Table 16a)")
setColWidths(phase_4_wb, "ATSI Conditions (Table 16a)", cols = 1, widths = 25)
addStyle(phase_4_wb, "ATSI Conditions (Table 16a)", style = cond.sty,
         rows = 1:nrow(cond.comp.atsi)+1, cols = 1:2, gridExpand = TRUE
)
freezePane(phase_4_wb, "ATSI Conditions (Table 16a)", firstActiveRow = 2, firstActiveCol = 3)
writeData(phase_4_wb, "ATSI Conditions (Table 16a)", cond.comp.atsi, headerStyle = hs1)

addWorksheet(phase_4_wb, "ATSI Min N (Table 16b)")
setColWidths(phase_4_wb, "ATSI Min N (Table 16b)", cols = 1, widths = 32)
addStyle(phase_4_wb, "ATSI Min N (Table 16b)", style = cond.sty,
         rows = 1:nrow(min.n.comp.atsi)+1, cols = 1:2, gridExpand = TRUE
)
freezePane(phase_4_wb, "ATSI Min N (Table 16b)", firstActiveRow = 2, firstActiveCol = 3)
writeData(phase_4_wb, "ATSI Min N (Table 16b)", min.n.comp.atsi, headerStyle = hs1)

##    Add a worksheet for Schools Ratings Indicator Distributions (19.)
tmp.sheet <- "Ratings Conditions (Table 19a)"
row.indx <- 1:nrow(acad.indc.dist)+1
addWorksheet(phase_4_wb, tmp.sheet)
setColWidths(phase_4_wb, tmp.sheet, cols = 1, widths = 25)
addStyle(phase_4_wb, tmp.sheet, style = cond.sty,
         rows = row.indx, cols = 1:2, gridExpand = TRUE
)
addStyle(phase_4_wb, tmp.sheet, style = achc.sty, rows = row.indx, cols = 3:4, gridExpand = TRUE)
addStyle(phase_4_wb, tmp.sheet, style = othc.sty, rows = row.indx, cols = 5:6, gridExpand = TRUE)
addStyle(phase_4_wb, tmp.sheet, style = smsc.sty, rows = row.indx, cols = 7:8, gridExpand = TRUE)
cor.cols <- grep("_Correl", names(acad.indc.dist))
for (rtg.cor in cor.cols) {
  conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = rtg.cor, rows = row.indx,
    rule = "<0.85", style = prob.flag
  )
}
conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = 4, rows = row.indx, rule = ">0.25", style = prob.flag)
conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = 6, rows = row.indx, rule = ">0.5", style = prob.flag)
conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = 8, rows = row.indx, rule = ">0.25", style = prob.flag)
freezePane(phase_4_wb, tmp.sheet, firstActiveRow = 2, firstActiveCol = 3)
writeData(phase_4_wb, tmp.sheet, acad.indc.dist, headerStyle = hs1)

tmp.sheet <- "Ratings Min N (Table 19b)"
row.indx <- 1:nrow(min.n.indc.dist)+1
cor.cols <- grep("_Correl", names(min.n.indc.dist))
rmse.cols <- grep("_RMSE", names(min.n.indc.dist))
addWorksheet(phase_4_wb, tmp.sheet)
setColWidths(phase_4_wb, tmp.sheet, cols = 1, widths = 32)
addStyle(phase_4_wb, tmp.sheet, style = cond.sty,
         rows = 1:nrow(min.n.indc.dist)+1, cols = 1:2, gridExpand = TRUE
)
addStyle(phase_4_wb, tmp.sheet, style = achc.sty, rows = row.indx, cols = 3)
addStyle(phase_4_wb, tmp.sheet, style = achr.sty, rows = row.indx, cols = 4)
addStyle(phase_4_wb, tmp.sheet, style = othc.sty, rows = row.indx, cols = 5)
addStyle(phase_4_wb, tmp.sheet, style = othr.sty, rows = row.indx, cols = 6)
addStyle(phase_4_wb, tmp.sheet, style = smsc.sty, rows = row.indx, cols = 7)
addStyle(phase_4_wb, tmp.sheet, style = smsr.sty, rows = row.indx, cols = 8)
cor.cols <- grep("_Correl", names(min.n.indc.dist))
for (rtg.cor in cor.cols) {
  conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = rtg.cor, rows = row.indx,
    rule = "<0.85", style = prob.flag
  )
}
conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = 4, rows = row.indx, rule = ">0.25", style = prob.flag)
conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = 6, rows = row.indx, rule = ">0.5", style = prob.flag)
conditionalFormatting(
    phase_4_wb, tmp.sheet, cols = 8, rows = row.indx, rule = ">0.25", style = prob.flag)
freezePane(phase_4_wb, tmp.sheet, firstActiveRow = 2, firstActiveCol = 3)
writeData(phase_4_wb, tmp.sheet, min.n.indc.dist, headerStyle = hs1)


##    Save Workbook
saveWorkbook(phase_4_wb, "./Data/Phase_4-Comparison_Metrics/Comparison_Metrics.xlsx", overwrite = TRUE)


# conditionalFormatting(phase_4_wb, tmp.sheet, cols=cor.cols, rows=1, type = "contains", rule = "Impact", style = impct.sty)
