
dBaseDT =
    function(
        long_data,
        configs,
        output.dir,
        output.behavior = "delete_matching" # c("overwrite", "error", "delete_matching")
    ){
        # Create output.dir if it doesn't already exist
        if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

        data.table::setkeyv(long_data, c("CONTENT_AREA", "GRADE", "YEAR"))

        for (K in seq(configs)) {
            config.iter <- configs[[K]]
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
                    )

                for (var in c("CONTENT_AREA", "GRADE", "YEAR")) {
                    mode(cohort.lookup[[var]]) <- mode(long_data[[var]])
                }

                arrow::write_dataset(
                    dataset = long_data[cohort.lookup],
                    path = output.dir,
                    format = "parquet",
                    existing_data_behavior = output.behavior,
                    partitioning =
                        c("VALID_CASE", "CONTENT_AREA", "GRADE", "YEAR")
                )
            }
        }
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
