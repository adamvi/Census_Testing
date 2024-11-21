require(data.table)
source("../functions/freadZIP.R")
`%w/o%` <- function(x, y) x[!x %in% y]

#####
###   Phase 1
#####

avi_p1 <-
    freadZIP(
      "Data/Cleaned_Data/Student_LongTestData_Georgia_2016-2022_AVI.csv.zip"
    )[YEAR < 2020]
ek_p1 <-
    fread(
      "Data/EK/Georgia - 1 - Cleaned and Merged Data/Student_LongTestData_Georgia_2016-2022_EK.csv"
    )[YEAR < 2020]

dim(avi_p1)
dim(ek_p1)
#  I end up with 3 more cases.  Why?
setkey(ek_p1, CONTENT_AREA, YEAR, GRADE, ID)
setkey(avi_p1, CONTENT_AREA, YEAR, GRADE, ID)

# anti-join:
avi_p1[!ek_p1]
ek_p1[!avi_p1]

# Inspect:
# One student took tests in both GRADE 5 & 6 in 2017, not an invalid case (duplicate within grade)
# This shouldn't impact the growth analyses enough to matter.

#####
###   Phase 2
#####

###   Growth Analyses
avi_p2a <-
    freadZIP(
      "Data/Phase_2-Student_Growth/Student_LongTestData_Georgia_2016-2019_AVI.csv.zip"
    )
ek_p2a <-
    fread(
      "Data/EK/Georgia - 2 - Simulated Study Conditions/Student_LongTestData_Georgia_2016-2019_EK_SGPs.csv"
    )

dim(avi_p2a)
dim(ek_p2a)


# tbla <- function(dt, col = "GRADE", row = "CONTENT_AREA", array = "YEAR", excl = NULL) {
#   tbl <- table(dt[, .(get(col), get(row), get(array))], exclude = excl)
#   names(attributes(tbl)[["dimnames"]]) <- c(col, row, array)
#   tbl
# }

tbla <- function(dt, col = "GRADE", row = "CONTENT_AREA", array = "YEAR", excl = NULL) {
  tbl <-
    table(dt[,
      .(eval(parse(text = col)),
        eval(parse(text = row)),
        eval(parse(text = array))
      )],
      exclude = excl
    )
  names(attributes(tbl)[["dimnames"]]) <- c(col, row, array)
  tbl
}

et <- tbla(ek_p2a, "GRADE", "is.na(SGP_Cnd_0)", "YEAR")
at <- tbla(avi_p2a, "GRADE", "is.na(SGP_Cnd_0)", "YEAR")

at[,,3]-et[,,3] # YEAR = 2018
at[,,4]-et[,,4] # YEAR = 2019

ast <-
  avi_p2a[YEAR %in% 2018:2019,
    .(S = sum(SGP_Cnd_0, na.rm = TRUE),
      M = mean(SGP_Cnd_0, na.rm = TRUE),
      Md = median(as.numeric(SGP_Cnd_0), na.rm = TRUE)
    ),
    by = c("CONTENT_AREA", "GRADE", "YEAR")
  ][!is.na(Md)]

est <-
  ek_p2a[YEAR %in% 2018:2019,
    .(S = sum(SGP_Cnd_0, na.rm = TRUE),
      M = mean(SGP_Cnd_0, na.rm = TRUE),
      Md = median(as.numeric(SGP_Cnd_0), na.rm = TRUE)
    ),
    by = c("CONTENT_AREA", "GRADE", "YEAR")
  ][!is.na(Md)]

identical(ast, est)


###   Growth Results Aggregation

cond <- "0"
yr <- 2018

fnm.avi <- "./Data/Phase_2-School_Summaries/School_Condition_"
fnm.ek <- "Data/EK/Georgia - 2 - Simulated Study Conditions/School_Condition_"

summary_checks <- list()
for (cond in c("0", "1a", "1b", "1c", "2", "3")) { # , "4" is way off due to random seed
  for (yr in 2018:2019) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    avi_ag <-
      fread(file = paste0(fnm.avi, cnd.nm, "_Georgia_", yr, "_AVI.csv"))
    # avi_ag[, c("ELA_PctProf", "Math_PctProf") := NULL]
    avi_ag[Group == "Multiracial", Group := "Multi"]
    avi_ag[Group == "Native American", Group := "Native"]
    if (cond == "1a") {
      avi_ag[, ELA_Improve := round(ELA_Improve, 3)]
      avi_ag[, Math_Improve := round(Math_Improve, 3)]
    }
    ek_ag <-
      fread(file = paste0(fnm.ek, cnd.nm, "_Georgia_", yr, "_EK.csv"))
    if (cond != "1a") {
      ek_ag[, c("ELA_Improve", "Math_Improve") := NULL]
      ek_ag[, ELA_MGP := round(ELA_MGP, 3)]
      ek_ag[, Math_MGP := round(Math_MGP, 3)]
    } else {
      ek_ag[, c("ELA_MGP", "Math_MGP", "ELA_GrowthN", "Math_GrowthN") := NULL]
      ek_ag[, ELA_Improve := round(ELA_Improve, 3)]
      ek_ag[, Math_Improve := round(Math_Improve, 3)]
    }

    setnames(ek_ag, "DSL_ID", "SchoolID")
    setcolorder(avi_ag, names(ek_ag))
    setkey(avi_ag, SchoolID, Group)
    setkey(ek_ag, SchoolID, Group)

    if (nrow(avi_ag) > nrow(ek_ag)) {
      # anti-join to get subset of additional cases
      tst <- avi_ag[!ek_ag]
      if (!all(is.na(tst[, 3:ncol(tst)]))) {
        summary_checks[[cond]][[as.character(yr)]][["extra_avi"]] <- tst
      }
      avi_ag <- avi_ag[ek_ag[, .(SchoolID, Group)]]
    } else if (nrow(ek_ag) > nrow(avi_ag)) {
      tst <- ek_ag[!avi_ag]
      if (!all(is.na(tst[, 3:ncol(tst)]))) {
        summary_checks[[cond]][[as.character(yr)]][["extra_ek"]] <- tst
      }
      ek_ag <- ek_ag[avi_ag[, .(SchoolID, Group)]]
    }

    for (nm in names(avi_ag) %w/o% c("SchoolID", "Group")) {
      ek_ag[, eval(nm) := as.character(get(nm))]
      avi_ag[, eval(nm) := as.character(get(nm))]
    }

    test_check <- (avi_ag != ek_ag) |> as.data.table()
    test_check[, matched_sum := rowSums(.SD, na.rm = TRUE)]

    avi_masked <- avi_ag[which(test_check$matched_sum != 0), ]
    ek_masked <- ek_ag[which(test_check$matched_sum != 0), ]

    for (nm in names(avi_ag) %w/o% c("SchoolID", "Group")) {
      ek_masked[, eval(nm) := as.numeric(get(nm))]
      avi_masked[, eval(nm) := as.numeric(get(nm))]
    }

    summary_checks[[cond]][[as.character(yr)]][["comp"]] <-
      cbind(
        avi_masked[, 1:2],
        avi_masked[, 3:ncol(avi_masked)] - ek_masked[, 3:ncol(ek_masked)]
      )
  }
}

summary_checks[["0"]][["2018"]][["comp"]] |>
  format(zero.print = "") |>
    print(quote = FALSE)

summary_checks[[cond]][[as.character(yr)]][["comp"]] |>
  format(zero.print = "") |>
  print(quote = FALSE)


summary(summary_checks[["3"]][["2019"]][["comp"]])

# chk.nms <-
#   c("ELA_TotalN", "ELA_ProfN", "ELA_GrowthN", "ELA_MGP",
#     "Math_TotalN", "Math_ProfN", "Math_GrowthN", "Math_MGP")
# ck <- "ELA_MGP"

    # tmp_mask <- test_check[matched_sum != 0]
    # for (nm in names(avi_masked) %w/o% c("SchoolID", "Group")) {
    #   avi_masked[which(tmp_mask[[nm]]), eval(nm) := NA]
    # }
    # test_check <-
    #   data.table(
    #     Condition = cond,
    #     Year = yr,
    #     ELA_TotalN = identical(ek_ag[["ELA_TotalN"]], avi_ag[["ELA_TotalN"]]),
    #     Math_TotalN = identical(ek_ag[["Math_TotalN"]], avi_ag[["Math_TotalN"]]),
    #     ELA_ProfN = identical(ek_ag[["ELA_ProfN"]], avi_ag[["ELA_ProfN"]]),
    #     Math_ProfN = identical(ek_ag[["Math_ProfN"]], avi_ag[["Math_ProfN"]]),
    #     ELA_GrowthN = identical(ek_ag[["ELA_GrowthN"]], avi_ag[["ELA_GrowthN"]]),
    #     Math_GrowthN = identical(ek_ag[["Math_GrowthN"]], avi_ag[["Math_GrowthN"]]),
    #     ELA_MGP = identical(ek_ag[["ELA_MGP"]], avi_ag[["ELA_MGP"]]),
    #     Math_MGP = identical(ek_ag[["Math_MGP"]], avi_ag[["Math_MGP"]])
    #   )

#####
###   Phase 3
#####

dir3.avi <- "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_"
dir3.ek <- "Data/EK/Georgia - 3 - Simulated Accountability Model/School_AcctRatings_Condition_"

# min.n <- 10

for (cond in c("0", "1a", "1b", "1c", "2", "3")) { # , "4"
  for (yr in 2018:2019) {
    for (min.n in c(10, 30, 50)) {
      message("Condition: ", cond, " | Year: ", yr, " | Min N: ", min.n)
      cnd.nm <-
        ifelse(
          cond %in% c("2", "3"),
          ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
          cond
        )
      avi_mod <-
        fread(
          file = paste0(dir3.avi, cnd.nm, "_", "Min", min.n, "_Georgia_", yr, "_AVI.csv")
        )
      avi_mod[Group == "Multiracial", Group := "Multi"]
      avi_mod[Group == "Native American", Group := "Native"]
      setkeyv(avi_mod, c("SchoolID", "Group"))

      ek_mod <-
        fread(
          file = paste0(dir3.ek, cond, "_", "Min", min.n, "_Georgia_", yr, "_EK.csv")
        ) |>
          setnames("DSL_ID", "SchoolID") |>
            setkeyv(c("SchoolID", "Group"))
      tst1 <- ek_mod[!avi_mod]
      tst2 <- ek_mod[avi_mod]
      message(nrow(tst1[!is.na(SumScore)]), " added rows")

      ach.z.diff <- tst2[abs(round(ACH_Z - i.ACH_Z, 2)) > 0] |> nrow()
      message(ach.z.diff, " different ACH_Z scores")
      if (ach.z.diff != 0) {
        table(round(tst2[, ACH_Z - i.ACH_Z], 2)) |> print()
      }

      oth.z.diff <- tst2[abs(round(Other_Z - i.Other_Z, 2)) > 0] |> nrow()
      message(oth.z.diff, " different Other_Z scores")
      if (oth.z.diff != 0) {
        table(round(tst2[, Other_Z - i.Other_Z], 2)) |> print()
      }

      pge.z.diff <- tst2[abs(round(ProgELP_Z - i.ProgELP_Z, 2)) > 0] |> nrow()
      message(pge.z.diff, " different ProgELP_Z scores")
      if (pge.z.diff != 0) {
        table(round(tst2[, ProgELP_Z - i.ProgELP_Z], 2)) |> print()
      }

      sqs.z.diff <- tst2[abs(round(SQSS_Z - i.SQSS_Z, 2)) > 0] |> nrow()
      message(sqs.z.diff, " different SQSS_Z scores")
      if (sqs.z.diff != 0) {
        table(round(tst2[, SQSS_Z - i.SQSS_Z], 2)) |> print()
      }

      sum.z.diff <- tst2[abs(round(SumScore - i.SumScore, 2)) > 0] |> nrow()
      message(sum.z.diff, " different SumScore scores")
      if (sum.z.diff != 0) {
        table(round(tst2[, SumScore - i.SumScore], 2)) |> print()
      }
      # tst2[abs(round(SumScore - i.SumScore, 2)) > 0, .(SchoolID, Group, SumScore, i.SumScore)]
    }
  }
}



phase1 <- # read_csv
  fread(paste0("./Data/EK/Georgia - 1 - Cleaned and Merged Data/School_AcctData_Georgia_", yr, "_EK.csv"))
phase2 <-
  fread("./Data/EK/Georgia - 2 - Simulated Study Conditions//School_Condition_1a_Georgia_2019_EK.csv")
state_indicators <-
  fread("Data/Phase_1-Cleaned_Data/School_AcctData_Georgia_2019_AVI.csv")
cond_summary_table <-
  fread("Data/Phase_2-School_Summaries/School_Condition_1a_Georgia_2019_AVI.csv")

av_data_18 <-
  fread("Data/Phase_1-Cleaned_Data/School_AcctData_Georgia_2018_AVI.csv")
av_data_18[Group == "Multiracial", Group := "Multi"]
av_data_18[Group == "Native American", Group := "Native"]
setkeyv(av_data_18, c("SchoolID", "Group"))

ek_data_18 <-
  fread("Data/EK/Georgia - 1 - Cleaned and Merged Data/School_AcctData_Georgia_2018_EK.csv") |>
    setnames("DSL_ID", "SchoolID") |>
      setkeyv(c("SchoolID", "Group"))

new_cases <- ek_data_18[!av_data_18]







sch_smry <- sch_smry_0E_10N[, (names(acct_rates)), with = FALSE]
