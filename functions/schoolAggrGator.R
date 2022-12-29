schoolAggrGator =
  function(
    data_table,
    growth.var,
    groups = c("SchoolID", "YEAR", "CONTENT_AREA")
  ) {
    aggr.names <-
      c("TotalN", "ProfN", "GrowthN", "MGP", "PctProf")
        # "MeanScore", "GrowthZ", "StatusZ"
    frmla <-
      paste0(
        paste(groups %w/o% "CONTENT_AREA", collapse = " + "),
        " ~ CONTENT_AREA"
      ) |> as.formula()
    data_table[,
      # the list of summaries can be reduced/increased/amended as needed:
      .(TotalN = .N,
        ProfN = sum(PROFICIENCY == 1L),
        GrowthN = sum(!is.na(get(growth.var))),
        MGP = round(mean(get(growth.var), na.rm = TRUE), 3),
        PctProf = round(mean(PROFICIENCY, na.rm = TRUE), 5)*100
        # GrowthZ = round(mean(qnorm(get(growth.var)/100), na.rm = TRUE), 3),
        # MeanScore = round(mean(Z_SCORE, na.rm = TRUE), 2),
        # StatusZ = round(mean(Z_PROFICIENCY, na.rm = TRUE), 3)
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


###  Condition 1a
cond1aAggrGator =
  function(
    data_table,
    group = NULL
  ) {
  smry_cnd_1a <-
    data_table[
        GRADE %in% c(5, 8),
        .(N = sum(!is.na(SCALE_SCORE)),
          MeanScore = mean(SCALE_SCORE, na.rm = TRUE),
          ScoreSD = sd(SCALE_SCORE, na.rm = TRUE)
        ),
        keyby = c("YEAR", "CONTENT_AREA", "GRADE", "SchoolID", group)
    ] |>
      setkeyv(
        c("SchoolID", "CONTENT_AREA", "YEAR", "GRADE")
      ) |>
      cfaTools::getShiftedValues(
          shift_group = c("SchoolID", "CONTENT_AREA", group),
          shift_variable = c("N", "MeanScore", "ScoreSD"),
          shift_amount = 1L
      )

  #  Subset the data for the two focus years:
  smry_cnd_1a <-
      smry_cnd_1a[YEAR %in% c(2018, 2019)]

  smry_cnd_1a[,
      ZDiff := (MeanScore - MeanScore_LAG_1) / ((ScoreSD*N + ScoreSD_LAG_1*N_LAG_1)/(N + N_LAG_1))
  ]

  #  Reshape by subject
  smry_cnd_1a[, GRADE := paste0("G", GRADE)]
  setkeyv(
    smry_cnd_1a,
    c("SchoolID", "CONTENT_AREA", "YEAR", "GRADE")
  )
  frmla <-
    paste0(
      paste(c("SchoolID", "YEAR", group), collapse = " + "),
      " ~ GRADE + CONTENT_AREA"
    ) |> as.formula()

  smry_cnd_1a <-
      dcast(
        data = smry_cnd_1a,
        formula = frmla,
        sep = "..",
        value.var = c("N", "ZDiff")
      )
  setnames(
    smry_cnd_1a,
    sapply(
      names(smry_cnd_1a),
      \(f) {
        tmp.name <- strsplit(f, "[.][.]")[[1]] |> rev() |> paste(collapse = "_")
        gsub("MATHEMATICS", "Math", tmp.name)
      }
    ) |> unlist()
  )

  smry_cnd_1a[,
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
  ]

  smry_cnd_1a[, (grep("ZDiff", names(smry_cnd_1a))) := NULL]

  if (!is.null(group)) {
    setnames(smry_cnd_1a, group, "Group")
  } else {
    smry_cnd_1a[, Group := "All"]
  }
  setcolorder(smry_cnd_1a, c("YEAR", "SchoolID", "Group"))

  return(smry_cnd_1a)
}
