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
