#####
###   Phase 4: Calculate Comparison Metrics
#####


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
    for (N in c(10, 30, 50)) {
      acct_ratings <-
        fread(
          file =
            paste0(
                "./Data/Accountability_Ratings/School_AcctRatings_Condition_",
                cnd.nm, "_Min", N, "_Georgia_", yr, "_AVI.csv"
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
            PctRatings = round((sum(!is.na(SumScore))) / .N, 5)
          ),
            keyby = "Group"
          ][,
              `Study Condition` :=
                  paste0("Sampling = ", cnd.nm, ", Minimum N = ", N)
          ][,
              Year := yr
          ] |>
            data.table::dcast(
              `Study Condition` + Year ~ Group,
              value.var = "PctRatings"
            )
      if (nrow(tmp_acct_rates) > 0L) {
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
}

setcolorder(
    ratings_percentages,
    c("Study Condition", "Year", "All", "EconDis", "EL", "SWD",
      "Asian", "Black", "Hispanic", "Native American", "Multiracial", "White")
)

setnames(
    ratings_percentages,
    names(ratings_percentages)[-(1:2)],
    paste0("PctRatings_", names(ratings_percentages)[-(1:2)])
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
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ] |>
        data.table::dcast(
            `Study Condition` + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Missing_"
        )
    # 4.b
    tmp_fp_misc <- rbindlist(list(
      data.table(
        table(tmp_missing[, EconDis], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ],
      data.table(
        table(tmp_missing[, SWD], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ],
      data.table(
        table(tmp_missing[, EL], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ]
    ))[
        grep(": Yes", V1)
    ][,
        V1 := gsub(": Yes", "", V1)
    ] |>
        data.table::dcast(
            `Study Condition` + Year ~ V1,
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
            table(tmp_missing[, GRADE, CONTENT_AREA], exclude = NULL), 1), 5),
          key = c("GRADE", "CONTENT_AREA")
        )
      ][ # 4.d
        tmp_missing[,
          .(AvgSS = round(mean(SCALE_SCORE, na.rm = TRUE), 1)),
          keyby = c("GRADE", "CONTENT_AREA")
        ]
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ][,
        CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
      ][,
        GRADE := paste0("Missing_G", GRADE)
      ] |>
        setnames("i.N", "%") |>
          data.table::dcast(
            `Study Condition` + Year ~ GRADE + CONTENT_AREA,
            value.var = c("N", "%", "AvgSS")
          )
    # 4.e
    tmp_fp_achlev <-
      data.table(
        table(tmp_missing[, ACHIEVEMENT_LEVEL], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ][,
        V1 := gsub(" Learner", "", V1)
      ][,
        V1 := gsub("Beginning", "PL1", V1)
      ][,
        V1 := gsub("Developing", "PL2", V1)
      ][,
        V1 := gsub("Proficient", "PL3", V1)
      ][,
        V1 := gsub("Distinguished", "PL4", V1)
      ] |>
        data.table::dcast(
            `Study Condition` + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Missing_"
        )
    freq_pct_race <- rbindlist(list(tmp_fp_race, freq_pct_race), fill = TRUE)
    freq_pct_misc <- rbindlist(list(tmp_fp_misc, freq_pct_misc), fill = TRUE)
    freq_pct_gdsubj <- rbindlist(list(tmp_fp_gdsubj, freq_pct_gdsubj), fill = TRUE)
    freq_pct_achlev <- rbindlist(list(tmp_fp_achlev, freq_pct_achlev), fill = TRUE)
  }
}

# wouldn't have Grade 3 - N_Missing_G3_ELA, %_Missing_G3_ELA
# Don't really understand 4.c

freq_pct_race |>
  setcolorder(
    c(1:2,
      lapply(
        gsub("N_Missing_|%_Missing_", "", names(freq_pct_race)[-(1:2)]) |> unique(),
        \(f) grep(f, names(freq_pct_race))
        ) |> unlist()
    )
  ) |>
    setkeyv(c("Study Condition",  "Year"))

freq_pct_misc |>
  setcolorder(
    c(1:2,
      unlist(lapply(c("EconDis", "SWD", "EL"), \(f) grep(f, names(freq_pct_misc)))))
  ) |>
    setkeyv(c("Study Condition",  "Year"))

freq_pct_gdsubj |>
  setcolorder(
    c(1:2,
      lapply(
        gsub("N_Missing_|%_Missing_|AvgSS_Missing_", "",
            names(freq_pct_gdsubj)[-(1:2)]) |> unique(),
        \(f) grep(f, names(freq_pct_gdsubj))
        ) |> unlist()
    )
  ) |>
    setkeyv(c("Study Condition",  "Year"))

freq_pct_achlev |>
  setcolorder(
    c(1:2,
      unlist(lapply(1:4, \(f) grep(paste0("PL", f), names(freq_pct_achlev)))))
  ) |>
    setkeyv(c("Study Condition",  "Year"))

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
          GRADE != 3,
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
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ] |>
        data.table::dcast(
            `Study Condition` + Year ~ V1,
            value.var = c("N", "%"),
            sep = "_Full_"
        )
    # 5.b
    tmp_fp_misc <- rbindlist(list(
      data.table(
        table(tmp_full[, EconDis], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ],
      data.table(
        table(tmp_full[, SWD], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ],
      data.table(
        table(tmp_full[, EL], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ]
    ))[
        grep(": Yes", V1)
    ][,
        V1 := gsub(": Yes", "", V1)
    ] |>
        data.table::dcast(
            `Study Condition` + Year ~ V1,
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
            table(tmp_full[, GRADE, CONTENT_AREA], exclude = NULL), 1), 5),
          key = c("GRADE", "CONTENT_AREA")
        )
      ][ # 5.d
        tmp_full[,
          .(AvgSS = round(mean(SCALE_SCORE, na.rm = TRUE), 1)),
          keyby = c("GRADE", "CONTENT_AREA")
        ]
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ][,
        CONTENT_AREA := gsub("MATHEMATICS", "Math", CONTENT_AREA)
      ][,
        GRADE := paste0("Full_G", GRADE)
      ] |>
        setnames("i.N", "%") |>
          data.table::dcast(
            `Study Condition` + Year ~ GRADE + CONTENT_AREA,
            value.var = c("N", "%", "AvgSS")
          )
    # 5.e
    tmp_fp_achlev <-
      data.table(
        table(tmp_full[, ACHIEVEMENT_LEVEL], exclude = NULL)
      )[,
        `%` := round(prop.table(N), 5)
      ][,
        `Study Condition` := paste0("Sampling = ", cnd.nm)
      ][,
        Year := yr
      ][,
        V1 := gsub(" Learner", "", V1)
      ][,
        V1 := gsub("Beginning", "PL1", V1)
      ][,
        V1 := gsub("Developing", "PL2", V1)
      ][,
        V1 := gsub("Proficient", "PL3", V1)
      ][,
        V1 := gsub("Distinguished", "PL4", V1)
      ] |>
        data.table::dcast(
            `Study Condition` + Year ~ V1,
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
      gsub("N_Full_|%_Full_", "", names(freq_pct_race)[-(1:2)]) |> unique(),
      \(f) grep(f, names(freq_pct_race))
    ) |> unlist()
  )
) |>
setkeyv(c("Study Condition",  "Year"))

freq_pct_misc |>
setcolorder(
  c(1:2,
    unlist(lapply(c("EconDis", "SWD", "EL"), \(f) grep(f, names(freq_pct_misc)))))
) |>
setkeyv(c("Study Condition",  "Year"))

freq_pct_gdsubj |>
setcolorder(
  c(1:2,
    lapply(
      gsub("N_Full_|%_Full_|AvgSS_Full_", "",
           names(freq_pct_gdsubj)[-(1:2)]) |> unique(),
      \(f) grep(f, names(freq_pct_gdsubj))
    ) |> unlist()
  )
) |>
setkeyv(c("Study Condition",  "Year"))

freq_pct_achlev |>
setcolorder(
  c(1:2,
    unlist(lapply(1:4, \(f) grep(paste0("PL", f), names(freq_pct_achlev)))))
) |>
setkeyv(c("Study Condition",  "Year"))

full_freq_pct_table <-
  freq_pct_race[freq_pct_misc][freq_pct_gdsubj][freq_pct_achlev]


# 6.
freq_pct_table <-
  missing_freq_pct_table[full_freq_pct_table]

var.order <-
    c("_EconDis$", "_SWD$", "_EL$",
      gsub("N_Full_|%_Full_", "", names(freq_pct_race)[-(1:2)]) |> unique(),
      gsub("N_Full_|%_Full_|AvgSS_Full_", "",
            names(freq_pct_gdsubj)[-(1:2)]) |> unique(),
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

# names(freq_pct_table)


##  Schools with No Summative Ratings
#
# 7. Filter the School-level Accountability Ratings file for the given
#    condition and year to only include records for which Group = "All"
#    AND SumScore is blank. This should result in a list of schools (DSL_IDs)
#    for which summative ratings could not be computed.

# I don't think this is right - we filtered schools based on N counts previously, so we'll need to figure out which schools were excluded in each condition.

# 8. Filter the Student-level Longitudinal Assessment file to only include
#    records with DSL_IDs that are in the list generated in the previous step.

# These should be filtered by relevant GRADE/CONTENT_AREA/YEAR as well

# 9. Aggregated the records from the previous step by school (DSL_ID) and
#    compute the following statistics for each school:
#   a. Total_N_NoSum = total number of students (i.e., unique student IDs)
#   b. Percentage distribution of the Race variable (i.e., %_Black_NoSum,
#      %_Hispanic_NoSum, %_White_NoSum, etc.)
#   c. Percentage distribution of the EL, SWD, and EconDis variables
#      (i.e., %_EL_NoSum, %_SWD_NoSum, %_EconDis_NoSum)
#   d. Percentage of students who are proficient by content area
#      (i.e, %_Proficient_ELA_NoSum and %_Proficient_Mathematics_NoSum)
#   e. Average scale score by grade and content area
#      (i.e., AvgSS_G3_ELA_NoSum, AvgSS_G4_ELA_NoSum, etc.)

excl_ratings <- data.table()
full_ratings <- data.table()

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
    for (N in c(10, 30, 50)) {
      # 7.
        acct_ratings <-
          fread(
            file = paste0(
                "./Data/Accountability_Ratings/School_AcctRatings_Condition_",
                cnd.nm, "_Min", N, "_Georgia_", yr, "_AVI.csv"
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
        excl_stu_n <- # 9.a
            tmp_excl[,
                .(Total_N_NoSum = length(unique(ID))),
                keyby = "SchoolID"
            ]
        excl_stu_race <- # 9.b
            tmp_excl[,
                .(N_NoSum = length(unique(ID))),
                keyby = c("SchoolID", "Race"),
            ][excl_stu_n][,
                PCT := round((N_NoSum / Total_N_NoSum), 5)
            ][,
                c("Total_N_NoSum", "N_NoSum") := NULL
            ] |>
                data.table::dcast(
                    SchoolID ~ Race,
                    value.var = "PCT"
                )
        setnames(
            excl_stu_race,
            names(excl_stu_race)[-1],
            paste0("%_", names(excl_stu_race)[-1], "_NoSum")
        )

        excl_stu_ed <- # 9.c
            tmp_excl[,
                .(`%_EconDis_NoSum` = round((sum(EconDis == "EconDis: Yes")/.N), 5)),
                keyby = "SchoolID",
            ]
        excl_stu_swd <-
            tmp_excl[,
                .(`%_SWD_NoSum` = round((sum(SWD == "SWD: Yes")/.N), 5)),
                keyby = "SchoolID",
            ]
        excl_stu_el <-
            tmp_excl[,
                .(`%_EL_NoSum` = round((sum(EL == "EL: Yes")/.N), 5)),
                keyby = "SchoolID",
            ]

        # 9.d
        excl_stu_achlev <-
            tmp_excl[,
                .(PCT = round((sum(PROFICIENCY == 1)/.N), 5)),
                keyby = c("SchoolID", "CONTENT_AREA"),
            ] |>
                data.table::dcast(
                    SchoolID ~ CONTENT_AREA,
                    value.var = "PCT"
                )
        setnames(
            excl_stu_achlev,
            c("SchoolID", "%_Proficient_ELA_NoSum", "%_Proficient_Mathematics_NoSum")
        )

        # 9.e
        excl_stu_mss <-
            tmp_excl[,
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
            excl_stu_mss,
            names(excl_stu_mss)[-1],
            paste0("AvgSS_", names(excl_stu_mss)[-1], "_NoSum")
        )

        # Combine into single table
        tmp_excl_ratings <-
            excl_stu_n[excl_stu_race][
                excl_stu_ed][excl_stu_swd][excl_stu_el][
                    excl_stu_achlev][excl_stu_mss][,
            `Study Condition` :=
                paste0("Sampling = ", cond, ", Minimum N = ", N)
        ][,
            Year := yr
        ]
        excl_ratings <-
            rbindlist(
                list(excl_ratings,
                     excl_stu_n[excl_stu_race][
                       excl_stu_ed][excl_stu_swd][excl_stu_el][
                         excl_stu_achlev][excl_stu_mss][,
                        `Study Condition` :=
                            paste0("Sampling = ", cond, ", Minimum N = ", N)
                    ][,
                        Year := yr
                    ]
                ),
                fill = TRUE
            )


# 10. Compute the same summary statistics for all records (i.e., unfiltered)
#     in the Student-level Longitudinal Assessment; that is: 
#   a. Total_N_Full = total number of students (i.e., unique student IDs)
#   b. Percentage distribution of the Race variable (i.e., %_Black_Full,
#      %_Hispanic_Full, %_White_Full, etc.)
#   c. Percentage distribution of the EL, SWD, and EconDis variables
#      (i.e., %_EL_Full, %_SWD_Full, %_EconDis_Full)
#   d. Percentage of students who are proficient by content area
#      (i.e, %_Proficient_ELA_Full and %_Proficient_Mathematics_Full)
#   e. Average scale score by grade and content area
#      (i.e., AvgSS_G3_ELA_Full, AvgSS_G4_ELA_Full, etc.)

#   I think this needs to be filtered somewhat to make sure we're comparing according to the condition (only including relevant GRADEs / CONTENT_AREAs)

        tmp_full <-
            Georgia_Data_LONG[grade_subject_lookup][YEAR == yr][,
                GRADE := as.character(GRADE)
            ]

      # 9.
        full_stu_n <- # 9.a
            tmp_full[,
                .(Total_N_Full = length(unique(ID))),
                keyby = "SchoolID"
            ]
        full_stu_race <- # 9.b
            tmp_full[,
                .(N_Full = length(unique(ID))),
                keyby = c("SchoolID", "Race"),
            ][full_stu_n][,
                PCT := round((N_Full / Total_N_Full), 5)
            ][,
                c("Total_N_Full", "N_Full") := NULL
            ] |>
                data.table::dcast(
                    SchoolID ~ Race,
                    value.var = "PCT"
                )
        setnames(
            full_stu_race,
            names(full_stu_race)[-1],
            paste0("%_", names(full_stu_race)[-1], "_Full")
        )

        full_stu_ed <- # 9.c
            tmp_full[,
                .(`%_EconDis_Full` = round((sum(EconDis == "EconDis: Yes")/.N), 5)),
                keyby = "SchoolID",
            ]
        full_stu_swd <-
            tmp_full[,
                .(`%_SWD_Full` = round((sum(SWD == "SWD: Yes")/.N), 5)),
                keyby = "SchoolID",
            ]
        full_stu_el <-
            tmp_full[,
                .(`%_EL_Full` = round((sum(EL == "EL: Yes")/.N), 5)),
                keyby = "SchoolID",
            ]

        # 9.d
        full_stu_achlev <-
            tmp_full[,
                .(PCT = round((sum(PROFICIENCY == 1)/.N), 5)),
                keyby = c("SchoolID", "CONTENT_AREA"),
            ] |>
                data.table::dcast(
                    SchoolID ~ CONTENT_AREA,
                    value.var = "PCT"
                )
        setnames(
            full_stu_achlev,
            c("SchoolID", "%_Proficient_ELA_Full", "%_Proficient_Mathematics_Full")
        )

        # 9.e
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
            paste0("AvgSS_", names(full_stu_mss)[-1], "_Full")
        )

        # Combine into single table
        tmp_full_ratings <-
            full_stu_n[full_stu_race][
                full_stu_ed][full_stu_swd][full_stu_el][
                    full_stu_achlev][full_stu_mss][,
            `Study Condition` :=
                paste0("Sampling = ", cond, ", Minimum N = ", N)
        ][,
            Year := yr
        ]
        full_ratings <-
            rbindlist(
                list(full_ratings,
                     full_stu_n[full_stu_race][
                       full_stu_ed][full_stu_swd][full_stu_el][
                         full_stu_achlev][full_stu_mss][,
                        `Study Condition` :=
                            paste0("Sampling = ", cond, ", Minimum N = ", N)
                    ][,
                        Year := yr
                    ]
                ),
                fill = TRUE
            )
    }
  }
}

# 11.  What is this table?

# setcolorder(
#     excl_ratings,
#     c("Study Condition", "Year",  "SchoolID", "Total_N_NoSum", 
#       lapply(
#         c("EconDis", "_EL_", "SWD", "Asian", "Black", "Hispanic",
#           "Native American", "Multiracial", "White"),
#         \(f) grep(f, names(excl_ratings), value = TRUE)
#       ) |> unlist()
#     )
# )


#####
###   Misc
#####

####

nosum_check <- full_stu_mss[SchoolID %in% excluded.schools] # Identical other than the "NoSum" / "Full" naming convention.
        

        acct_ratings_ext <-
          completeDT(acct_ratings_ext, cols = c("SchoolID", "Group"))

      tmp_acct_rates <-
        acct_ratings_ext[,
          .(NumSchools = .N,
            NumRatings = sum(!is.na(SumScore)),
            PctRatings = round((sum(!is.na(SumScore))) / .N, 5)
          ),
            keyby = "Group"
          ][,
              `Study Condition` :=
                  paste0("Sampling = ", cond, ", Minimum N = ", N)
          ][,
              Year := yr
          ] |>
            data.table::dcast(
              `Study Condition` + Year ~ Group,
              value.var = "PctRatings"
            )
      if (nrow(tmp_acct_rates) > 0L) {
        excl_ratings <-
          rbindlist(
            list(
                excl_ratings,
                tmp_acct_rates
            ),
            fill = TRUE
          )
      }