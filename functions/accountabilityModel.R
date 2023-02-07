accountabilityModel =
  function(
    condition,
    min.n,
    state_indicators,
    cond_summary_table
  ) {
    ###   Merge and Filter
    setkey(state_indicators, SchoolID, Group)
    setkey(cond_summary_table, SchoolID, Group)

    tmp_mf_tbl <-
      cond_summary_table[state_indicators][
          !(is.na(ELA_PartRate) | is.na(Math_PartRate)) &
          ELA_TotalN > min.n &    # 2.a
          Math_TotalN > min.n     # 2.b
      ]
    if (toupper(condition) != "1A") {
      tmp_mf_tbl <-
        tmp_mf_tbl[
            ELA_GrowthN > min.n & # 2.c.i
            Math_GrowthN > min.n  # 2.c.ii
        ]
    }

    ###   Compute Academic Achievement Indicator Scores
    tmp_mf_tbl[,
      ELA_ProfRate := ELA_ProfN / ELA_TotalN                     # 3.a
    ][ELA_PartRate <= 95, 
      ELA_ProfRate := ELA_ProfRate * ((ELA_PartRate/100)/0.95)   # 3.b
    ][,
      Math_ProfRate := Math_ProfN / Math_TotalN                  # 4.a
    ][Math_PartRate <= 95, 
      Math_ProfRate := Math_ProfRate * ((ELA_PartRate/100)/0.95) # 4.b
    ][,
      ACH_Score :=                                               # 5.a
        ((ELA_ProfRate * ELA_TotalN) + (Math_ProfRate * Math_TotalN)) / 
                         (ELA_TotalN + Math_TotalN)
    ]

    ###   Compute Other Academic Indicator Scores 
    if (toupper(condition) == "1A") {   # 6.a
      tmp_mf_tbl[,
        Other_Score :=
          (ELA_Improve*ELA_TotalN + Math_Improve*Math_TotalN) / (ELA_TotalN + Math_TotalN)
      ]
    } else {                            # 7.a
      tmp_mf_tbl[,
        Other_Score :=
          (ELA_MGP*ELA_GrowthN + Math_MGP*Math_GrowthN) / (ELA_GrowthN + Math_GrowthN)
      ]
    }

    ###   Standardize Indicator Scores and Compute Summative Ratings
    # 8. Compute the mean and standard deviation of each accountability indicator
    #    across all schools ("All Students" records only).
    #    8.b.i through 8.b.x:
    indc_smry <-
      tmp_mf_tbl[Group == "All",
        .(ACH_Mean = mean(ACH_Score, na.rm = TRUE),
          ACH_SD = sd(ACH_Score, na.rm = TRUE),
          Other_Mean = mean(Other_Score, na.rm = TRUE),
          Other_SD = sd(Other_Score, na.rm = TRUE),
          ProgELP_Mean = mean(ProgELP, na.rm = TRUE),
          ProgELP_SD = sd(ProgELP, na.rm = TRUE),
          SQSS_Mean = mean(SQSS, na.rm = TRUE),
          SQSS_SD = sd(SQSS, na.rm = TRUE)
          # GradRate_Mean = mean(GradRate, na.rm = TRUE),
          # GradRate_SD = sd(GradRate, na.rm = TRUE)
        )
      ]

    # 9. Standardize the indicator scores for each school and student group.
    tmp_mf_tbl[,
      ACH_Z := (ACH_Score - indc_smry$ACH_Mean)/indc_smry$ACH_SD
    ][,
      Other_Z := (Other_Score - indc_smry$Other_Mean)/indc_smry$Other_SD
    ][,
      ProgELP_Z := (ProgELP - indc_smry$ProgELP_Mean)/indc_smry$ProgELP_SD
    ][,
      SQSS_Z := (SQSS - indc_smry$SQSS_Mean)/indc_smry$SQSS_SD
    ]#[,
    #   GradRate_Z := (GradRate_Score - indc_smry$GradRate_Mean)/indc_smry$GradRate_SD
    # ]

    # 10. Compute the summative rating score for each school and student group.
    ##  Elementary/Middle Schools
    tmp_mf_tbl[is.na(ProgELP_Z),
      SumScore :=
        sum(c(ACH_Z * 0.43, Other_Z * 0.43, SQSS_Z * 0.14)),
      by = c("SchoolID", "Group")
    ]
    tmp_mf_tbl[!is.na(ProgELP_Z),
      SumScore :=
        sum(c(ACH_Z * 0.35, Other_Z * 0.35, ProgELP_Z * 0.18, SQSS_Z * 0.12)),
      by = c("SchoolID", "Group")
    ]

    ##  High Schools (Not implemented)

    ###   Identify Schools for Support and Improvement
    # 11.a/b & 12.a/b (only calculate 5th percentile cutoff -P5- for binary indicator)
    P5_Lookup <-
      tmp_mf_tbl[,
        .(P5 = quantile(x = SumScore, probs = 0.05, na.rm = TRUE)),
        keyby = "Group"
    ]
    setkey(tmp_mf_tbl, Group)
    tmp_mf_tbl <- P5_Lookup[tmp_mf_tbl]
    
    # 11.c - 13
    tmp_mf_tbl[,
      Low5Pct := ifelse(SumScore < P5, 1, 0)
    ][!is.na(Low5Pct),
      CSI := ifelse(Low5Pct == 1 & Group == "All", 1, 0)
    ][!is.na(Low5Pct),
      ATSI := ifelse(Low5Pct == 1 & Group != "All", 1, 0)
    ][,
      P5 := NULL
    ]

    # Format and return results
    setcolorder(tmp_mf_tbl, c("SchoolID", "Group"))
    setkey(tmp_mf_tbl, SchoolID, Group)

    tmp_mf_tbl[,
      .(SchoolID, Group, ACH_Z, Other_Z, ProgELP_Z, SQSS_Z, SumScore, Low5Pct, CSI, ATSI)
    ][]
  }
