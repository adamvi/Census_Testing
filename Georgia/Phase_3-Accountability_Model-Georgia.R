#####
###   Phase 3: Simulate Accountability Model (Processing)
#####

require(data.table)
`%w/o%` = function(x, y) x[!x %in% y]

state_acct_data_18 <-
  fread("Data/Cleaned_Data/School_AcctData_Georgia_2018_EK.csv")

state_acct_data_18[,
  ELA_PartRate := as.numeric(ELA_PartRate)
][,
  Math_PartRate := as.numeric(Math_PartRate)
][,
  ProgELP := as.numeric(ProgELP)
][,
  Beyond_The_Core  := as.numeric(Beyond_The_Core)
][,
  Literacy := as.numeric(Literacy)
][,
  Student_Attendance := as.numeric(Student_Attendance)
]

# How should NA in indicators be dealt with?
# Impute where missing
sqss.vars <- c("Literacy", "Beyond_The_Core", "Student_Attendance")
for (sqss in sqss.vars) {
  tmp.data <-
    state_acct_data_18[!(is.na(ELA_PartRate) | is.na(Math_PartRate))]
  # tmp.fmla <-
  #   as.formula(
  #     paste(sqss, "~", paste(sqss.vars %w/o% sqss, collapse = "+"), "+ Group")
  #   )
  # tmp.lm <- lm(formula = tmp.fmla, data = tmp.data)
  # tmp.pred <- predict.lm(object = tmp.lm, newdata = tmp.data)
  # tmp.pred[tmp.pred < 0 & !is.na(tmp.pred)] <- 0
  # tmp.lm <- lm(formula = as.formula(paste(sqss, "~ Group")), data = tmp.data)
  # tmp.pred2 <- predict.lm(object = tmp.lm, newdata = tmp.data) # predicted group means
  # tmp.pred[is.na(tmp.pred)] <- tmp.pred2[is.na(tmp.pred)]
  tmp.rq <-
    quantreg::rq(
      formula = as.formula(paste(sqss, "~ Group")),
      data = tmp.data
    )
  tmp.pred <- predict(object = tmp.rq, newdata = tmp.data)
  tmp.pred[tmp.pred < 0 & !is.na(tmp.pred)] <- 0
  state_acct_data_18[
    !(is.na(ELA_PartRate) | is.na(Math_PartRate)),
    TMP_PRED := round(tmp.pred, 3)
  ][
    is.na(get(sqss)),
    eval(sqss) := TMP_PRED
  ][,
    TMP_PRED := NULL
  ]
}

state_acct_data_18[,
  SQSS := rowMeans(.SD, na.rm = TRUE),   # SQSS = NA if ALL are NA, mean of those available
  # SQSS := rowSums(.SD, na.rm = FALSE)/3, # SQSS = NA if any are NA?
  # SQSS := rowSums(.SD, na.rm = TRUE)/3,    # Treated as 0? Penalized for NA
  .SDcols = (sqss.vars)
]

state_acct_data_19 <-
  fread("Data/Cleaned_Data/School_AcctData_Georgia_2019_EK.csv")

state_acct_data_19[,
  ELA_PartRate := as.numeric(ELA_PartRate)
][,
  Math_PartRate := as.numeric(Math_PartRate)
][,
  ProgELP := as.numeric(ProgELP)
][,
  Beyond_The_Core  := as.numeric(Beyond_The_Core)
][,
  Literacy := as.numeric(Literacy)
][,
  Student_Attendance := as.numeric(Student_Attendance)
]

# Impute 2019 where missing
for (sqss in sqss.vars) {
  tmp.data <-
    state_acct_data_19[!(is.na(ELA_PartRate) | is.na(Math_PartRate))]
  tmp.rq <-
    quantreg::rq(
      formula = as.formula(paste(sqss, "~ Group")),
      data = tmp.data
    )
  tmp.pred <- predict(object = tmp.rq, newdata = tmp.data)
  tmp.pred[tmp.pred < 0 & !is.na(tmp.pred)] <- 0
  state_acct_data_19[
    !(is.na(ELA_PartRate) | is.na(Math_PartRate)),
    TMP_PRED := round(tmp.pred, 3)
  ][
    is.na(get(sqss)),
    eval(sqss) := TMP_PRED
  ][,
    TMP_PRED := NULL
  ]
}

state_acct_data_19[,
  SQSS := rowMeans(.SD, na.rm = TRUE),  #  Keep this option per LK (2/1/23)
  # SQSS := rowSums(.SD, na.rm = TRUE)/3,
  .SDcols = c("Beyond_The_Core", "Literacy", "Student_Attendance")
]

# ProgELP only in "EL" rows in 2019.  Who should this count for? All/EL only? Every Group?
ProgELP_Lookup <-
  state_acct_data_19[
    Group == "EL" & !is.na(ProgELP),
      .(SchoolID, Group, ProgELP)
  ][,
    Group := "All"
  ]

setkey(ProgELP_Lookup, SchoolID, Group)
setkey(state_acct_data_19, SchoolID, Group)
state_acct_data_19 <- ProgELP_Lookup[state_acct_data_19]
state_acct_data_19[
  !is.na(i.ProgELP), ProgELP := i.ProgELP
][,
  i.ProgELP := NULL
]



#' # Example with 2018 Condition 0
#'
#' Uncomment and run to walk through example code which was the basis for
#' the `accountabilityModel` function used below.
#' 
#+ phase3-ex, include = FALSE, echo = FALSE, purl = FALSE
# sch_smry_0 <-
#   fread("Data/Phase_2-School_Summaries/School_Condition_0_Georgia_2018_AVI.csv")

# ###   Merge and Filter
# ##    1. Merge
# setkey(state_acct_data_18, SchoolID, Group)
# setkey(sch_smry_0, SchoolID, Group)

# sch_smry_0E <- sch_smry_0[state_acct_data_18]

# ##    2. Filter (N-sizes)

# min.n <- 10  #  30  #  50

# sch_smry_0E_10N <-
#   sch_smry_0E[
#     !(is.na(ELA_PartRate) | is.na(Math_PartRate)) &
#     ELA_ProfN > min.n &   # a.
#     Math_ProfN > min.n &  # b.
#     ELA_GrowthN > min.n & # c.i.
#     Math_GrowthN > min.n  # c.ii.
#   ]

# ###   Compute Academic Achievement Indicator Scores
# # 3. Compute the ELA proficiency rate in the focus year:
# #   a. If ELA_PartRate  95%, then ELA_ProfRate = ELA_ProfN / ELA_TotalN
# #   b. Otherwise, ELA_ProfRate =  (ELA_ProfN / ELA_TotalN) * (ELA_PartRate / 95%)
# # ...  4. => same for math
# # 5. Compute the Academic Achievement indicator score as the weighted
# #    average of the ELA and mathematics proficiency rate in the focus year:
# #   a.  ACH_Score = (ELA_ProfRate * ELA_TotalN + Math_ProfRate * Math_TotalN) / (ELA_TotalN + Math_TotalN)

# sch_smry_0E_10N[,
#   ELA_ProfRate := ELA_ProfN / ELA_TotalN                     # 3.a
# ][ELA_PartRate <= 95, 
#   ELA_ProfRate := ELA_ProfRate * ((ELA_PartRate/100)/0.95)   # 3.b
# ][,
#   Math_ProfRate := Math_ProfN / Math_TotalN                  # 4.a
# ][Math_PartRate <= 95, 
#   Math_ProfRate := Math_ProfRate * ((ELA_PartRate/100)/0.95) # 4.b
# # XXX  `(*_PartRate / 95%)` piece seems wrong? Up-weights the *_PartRate. e.g. 0.94/0.95 = ~0.99
# ][,
#   ACH_Score :=                                               # 5.a
#     ((ELA_ProfRate * ELA_TotalN) + (Math_TotalN * Math_ProfRate)) / 
#                     (ELA_TotalN  +  Math_TotalN)
# ]

# ###   Compute Other Academic Indicator Scores 
# # 6. For Condition 1a, compute the Other Academic indicator score as the
# #    weighted average of the ELA and mathematics improvement in the focus year
# #   a. Other_Score = (ELA_Improve * ELA_TotalN + Math_Improve * Math_TotalN) / (ELA_TotalN + Math_TotalN)
# #
# # 7. For all other conditions, compute the Other Academic indicator score as
# #    the weighted average of the ELA and mathematics MGPs in the focus year
# #   a. Other_Score = (ELA_MGP*ELA_GrowthN + Math_MGP*Math_GrowthN) / (ELA_GrowthN + Math_GrowthN)

# sch_smry_0E_10N[,
#   Other_Score :=
#     ((ELA_MGP * ELA_GrowthN) + (Math_GrowthN * Math_MGP)) /
#                (ELA_GrowthN  +  Math_GrowthN)
# ]


# ###   Standardize Indicator Scores and Compute Summative Ratings 
# # 8. Compute the mean and standard deviation of each accountability indicator
# #    across all schools (that is, across every "All Students" records).
# # * Only include records with Group = "All".
# # * Calculate the following across all records:
# #   - ACH_Mean = grand mean of ACH_Score (i.e., the average ACH_Score across all schools)
# #   - ACH_SD = standard deviation of ACH_Score (i.e. across all schools)
# #   - Other_Mean = grand mean of Other_Score (i.e., the average Other_Score across all schools)
# #   - Other_SD = standard deviation of Other_Score (i.e. across all schools)
# #   - ProgELP_Mean = grand mean of ProgELP (i.e., the average ProgELP across all schools that have a ProgELP score)
# #   - ProgELP_SD = standard deviation of ProgELP (i.e. across all schools that have a ProgELP score)
# #   - SQSS_Mean = grand mean of SQSS (i.e., the average SQSS across all schools)
# #   - SQSS_SD = standard deviation of SQSS (i.e. across all schools)
# #   - GradRate_Mean = grand mean of GradRate (i.e., the average GradRate across all high schools)
# #   - GradRate_SD = standard deviation of GradRate (i.e. across all high schools)

# indc_smry <-
#   sch_smry_0E_10N[Group == "All",
#     .(ACH_Mean = mean(ACH_Score, na.rm = TRUE),
#       ACH_SD = sd(ACH_Score, na.rm = TRUE),
#       Other_Mean = mean(Other_Score, na.rm = TRUE),
#       Other_SD = sd(Other_Score, na.rm = TRUE),
#       ProgELP_Mean = mean(ProgELP, na.rm = TRUE),
#       ProgELP_SD = sd(ProgELP, na.rm = TRUE),
#       SQSS_Mean = mean(SQSS, na.rm = TRUE),
#       SQSS_SD = sd(SQSS, na.rm = TRUE)
#       # GradRate_Mean = mean(GradRate, na.rm = TRUE),
#       # GradRate_SD = sd(GradRate, na.rm = TRUE)
#     )
#   ]

# # 9. Standardize the indicator scores for each school and student group.
# #    That is, for each record in the dataset, compute the following:
# # * ACH_Z = (ACH_Score - ACH_Mean)/ACH_SD
# # * Other_Z = (Other_Score - Other_Mean)/Other_SD
# # * ProgELP_Z = (ProgELP - ProgELP_Mean)/ProgELP_SD
# # * SQSS_Z = (SQSS - SQSS_Mean)/SQSS_SD
# # * GradRate_Z = (GradRate_Score - GradRate_Mean)/GradRate_SD

# sch_smry_0E_10N[,
#   ACH_Z := (ACH_Score - indc_smry$ACH_Mean)/indc_smry$ACH_SD
# ][,
#   Other_Z := (Other_Score - indc_smry$Other_Mean)/indc_smry$Other_SD
# ][,
#   ProgELP_Z := (ProgELP - indc_smry$ProgELP_Mean)/indc_smry$ProgELP_SD
# ][,
#   SQSS_Z := (SQSS - indc_smry$SQSS_Mean)/indc_smry$SQSS_SD
# ]#[,
# #   GradRate_Z := (GradRate_Score - indc_smry$GradRate_Mean)/indc_smry$GradRate_SD
# # ]

# # 10. Compute the summative rating score for each school and student group.
# #     That is, for each record in the dataset, calculate a weighted composite
# #     score, `SumScore`.
# # * SumScore = 0.35*ACH_Z + 0.35*Other_Z + 0.18*ProgELP_Z + 0.12*SQSS_Z 

# ##  Elementary/Middle Schools
# sch_smry_0E_10N[is.na(ProgELP_Z),
#   SumScore :=
#     sum(c(ACH_Z * 0.43, Other_Z * 0.43, SQSS_Z * 0.14)),
#   by = c("SchoolID", "Group")
# ]
# sch_smry_0E_10N[!is.na(ProgELP_Z),
#   SumScore :=
#     sum(c(ACH_Z * 0.35, Other_Z * 0.35, ProgELP_Z * 0.18, SQSS_Z * 0.12)),
#   by = c("SchoolID", "Group")
# ]

# ##  High Schools (Not implemented)


# ###   Identify Schools for Support and Improvement 
# # 11. Identify schools for CSI using the "All Students" records.
# #   a. Only include records with Group = "All".
# #   b. For each record, compute the percentile rank, PRank, of its SumScore
# #      value compared to all records.
# #   c. If PRank  0.05 (that is the 5th percentile rank or less), then
# #      Low5Pct = 1 and CSI = 1; otherwise Low5Pct = 0 and CSI = 0.
# # 12. Determine the lowest 5% of schools for each student group. That is,
# #     repeat the following steps for each student group, <Group>, where
# #     <Group> is not the "All Students" group.
# #   a. Only include records with Group = <Group>, (where <Group> != "All").
# #   b. For each record, compute the percentile rank, PRank, of its SumScore
# #      value compared to all records.
# #   c. If PRank_<Group>  0.05, then Low5Pct = 1; otherwise Low5Pct = 0.
# # 13. Identify schools for ASTI based on the Low5Pct values for all its student groups.
# #   a. For each school (i.e., every unique DSL_ID), if Low5Pct = 1 for any of
# #     <Group>, where <Group> != "All", then ATSI = 1; otherwise, ASTI = 0.

# P5_Lookup <-
#   sch_smry_0E_10N[,
#     .(P5 = quantile(x = SumScore, probs = 0.05, na.rm = TRUE)),
#     keyby = "Group"
# ]
# setkey(sch_smry_0E_10N, Group)
# sch_smry_0E_10N <- P5_Lookup[sch_smry_0E_10N]

# sch_smry_0E_10N[,
#   Low5Pct := ifelse(SumScore < P5, 1, 0)
# ][,
#   P5 := NULL
# ]
# # round(prop.table(table(sch_smry_0E_10N[, Low5Pct, Group], exclude = NULL), 1) * 100, 2)
# sch_smry_0E_10N[,
#   CSI := ifelse(Low5Pct == 1 & Group == "All", 1, 0)
# ][,
#   ATSI := ifelse(Low5Pct == 1 & Group != "All", 1, 0)
# ]


###  Calculate accountability ratings for each condition and output files

#  Create a subdirectory for accountability ratings
if (!dir.exists("./Data/Phase_3-Accountability_Ratings"))
     dir.create("./Data/Phase_3-Accountability_Ratings")

#  Read in the custom function
source("../functions/accountabilityModel.R")

#  Calculate and output
fprefix <- "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_"

for (cond in c("0", "1a", "1b", "1c", "2", "3", "4")) {
  for (yr in 2018:2019) {
    cnd.nm <-
      ifelse(
        cond %in% c("2", "3"),
        ifelse(yr == 2018, paste0(cond, "_E"), paste0(cond, "_O")),
        cond
      )
    tmp_cnd_data <-
      fread(
        file =
          paste0("./Data/Phase_2-School_Summaries/School_Condition_",
                 cnd.nm, "_Georgia_", yr, "_AVI.csv"
          )
      )

    for (N in c(10, 30, 50)) {
      tmp_acct_rates <-
        accountabilityModel(
          condition = cond,
          min.n = N,
          state_indicators =
            switch(as.character(yr),
              "2018" = state_acct_data_18,
              "2019" = state_acct_data_19
            ),
          cond_summary_table = tmp_cnd_data
        )
      if (nrow(tmp_acct_rates) > 0L) {
        fwrite(
          x = tmp_acct_rates,
          file = paste0(fprefix, cnd.nm, "_Min", N, "_Georgia_", yr, "_AVI.csv")
        ) # School_AcctRatings_Condition_<c>_Min<n> _<State>_<FYear>_<init>.csv
      }
    }
  }
}




###   Misc. work that didn't pan out 

# source("../functions//getQuantcut.R")

# Percentile_Lookup <-
#   sch_smry_0E_10N[,
#     as.list(getQuantcut(SumScore, quantiles = 1:99/100)),
#     keyby = "Group"
#   ]

# sch_smry_0E_10N[,
#   PRank := getQuantcut(SumScore, quantiles = 1:99/100),
#   by = "Group"
# ]


# tst <-
#   accountabilityModel(
#     condition = "0",
#     min.n = 10,
#     state_indicators = state_acct_data_18,
#     cond_summary_table = sch_smry_0
#   )

# sts <- fread("Data/Phase_2-School_Summaries/School_Condition_0_Georgia_2019_AVI.csv")
# tst2 <-
#   accountabilityModel(
#     condition = "0",
#     min.n = 10,
#     state_indicators = state_acct_data_19,
#     cond_summary_table = sts
      
#   )

# weighted.mean(x, w, …, na.rm = FALSE)

# w1 = c(.43, .43, .14)
# tst <- c( -1.0539309, -0.5602951, -0.65007310)
# mean(tst)
# weighted.mean(tst, w1)

# tst %*% w1
# sum(tst * w1)

# sch_smry_0E_10N[is.na(ProgELP_Z),
#   SumScore := rowSums(.SD*w1),
#   .SDcols = c("ACH_Z", "Other_Z", "SQSS_Z")
# ]
