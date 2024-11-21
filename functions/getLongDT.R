
getLongDT =
    function(
        db.con,
        config
    ){
        if (length(config) > 1) long_data <- data.table()

        for (K in seq(config)) {
            config.iter <- config[[K]]
            names(config.iter) <- gsub("^sgp[.]", "", names(config.iter))

            for (G in seq(config.iter[["grade.sequences"]])) {
                cohort.iter <- config.iter
                cohort.iter[["grade.sequences"]] <-
                    cohort.iter[["grade.sequences"]][[G]]
                grade.length <- length(cohort.iter[["grade.sequences"]])
                cohort.lookup <-
                    data.table::data.table(
                        CONTENT_AREA =
                            tail(cohort.iter[["content.areas"]], grade.length),
                        GRADE = cohort.iter[["grade.sequences"]],
                        YEAR = tail(cohort.iter[["panel.years"]], grade.length)
                    ) |>
                        arrow::arrow_table()

                cohort.lookup <-
                    cohort.lookup$cast(
                        target_schema = db.con$schema[c("CONTENT_AREA", "GRADE", "YEAR")]
                    )

                cohort_data <- db.con |>
                    semi_join(cohort.lookup) |>
                        as.data.table(key = getKey(db.con))

                if (length(config) > 1) {
                    data.table::setkeyv(cohort_data, getKey(cohort_data))
                    long_data <- rbindlist(list(long_data, cohort_data))
                }
            }
        }
        if (length(config) > 1) long_data[] else cohort_data[]
    }

`%w/o%` <- function(x, y) x[!x %in% y]

`getKey` <- function(data) {
    if ("YEAR_WITHIN" %in% names(data)) {
        return(
            c("VALID_CASE", "CONTENT_AREA", "YEAR",
              "GRADE", "ID", "YEAR_WITHIN"
            )
        )
    } else {
        return(c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "ID"))
    }
}
