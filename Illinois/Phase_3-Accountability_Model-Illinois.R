#####
###   Phase 3: Simulate Accountability Model (Processing)
#####

require(data.table)
`%w/o%` = function(x, y) x[!x %in% y]

state_acct_data_18 <-
  fread("Data/State_Files/School_AcctData_IL_EM_2018_LK.csv")

setnames(
  state_acct_data_18,
  c("School_ID", "ELP_ProfRate"),
  c("SchoolID", "ProgELP")
)

# How should NA in indicators be dealt with?
# Impute where missing
sqss.vars <- "ChronicAbsent"
for (sqss in sqss.vars) {
  state_acct_data_18[!(is.na(ELA_PartRate) | is.na(Math_PartRate)),
      TMP_M := median(get(sqss), na.rm = TRUE), by = "Group"
  ][is.na(get(sqss)),
      eval(sqss) := round(TMP_M, 3)
  ][, TMP_M := NULL
  ]
}

state_acct_data_18[,
  SQSS := rowMeans(.SD, na.rm = TRUE),
  .SDcols = (sqss.vars)
]

state_acct_data_19 <-
  fread("Data/State_Files/School_AcctData_IL_EM_2019_LK.csv")

setnames(
  state_acct_data_19,
  c("School_ID", "ELP_ProfRate"),
  c("SchoolID", "ProgELP")
)

# Impute 2019 where missing
for (sqss in sqss.vars) {
  state_acct_data_19[,
      TMP_M := median(get(sqss), na.rm = TRUE), by = "Group"
  ][is.na(get(sqss)),
      eval(sqss) := round(TMP_M, 3)
  ][, TMP_M := NULL
  ]
}

state_acct_data_19[,
  SQSS := rowMeans(.SD, na.rm = TRUE),  #  Keep this option per LK (2/1/23)
  .SDcols = (sqss.vars)
]


###  Calculate accountability ratings for each condition and output files

#  Create a subdirectory for accountability ratings
if (!dir.exists("./Data/Phase_3-Accountability_Ratings"))
     dir.create("./Data/Phase_3-Accountability_Ratings")

#  Read in the custom function
source("../functions/accountabilityModel.R")

#  Calculate and output
fprefix <- "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition_"

for (cond in c("0", "0v2", "1a", "1b", "1c", "2", "3", "4", "4v2")) {
  for (yr in 2018:2019) {
    if (yr == 2018 && cond == "1a") next
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
                 cnd.nm, "_Illinois_", yr, "_AVI.csv"
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
          file = paste0(fprefix, cnd.nm, "_Min", N, "_Illinois_", yr, "_AVI.csv")
        ) # School_AcctRatings_Condition_<c>_Min<n> _<State>_<FYear>_<init>.csv
      }
    }
  }
}
