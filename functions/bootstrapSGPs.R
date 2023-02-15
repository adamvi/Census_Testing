
bootstrapSGPs =
    function(
        sgp_data, config, state.abbr, fyear, workers,
        bootstrap.n = 100, coef_matrices = NULL,
        output.dir = "./Data/Phase_4-Metric_Bootstrap/Student_Growth"
    ){
        # Create a folder "Bootstrap" if it doesn't already exist
        if (!dir.exists(output.dir)) dir.create(output.dir, recursive = TRUE)

        for (boot.n in seq(bootstrap.n)) {
            resampd_data <- resampleData(sgp_data, config)
            # table(resampd_data[, .(GRADE, YEAR), CONTENT_AREA])

            SGP_object <-
                prepareSGP(
                    data = resampd_data,
                    state = state.abbr,
                    create.additional.variables = FALSE
                )
            SGP_object@SGP[["Coefficient_Matrices"]] <- coef_matrices

            SGP_object <-
                abcSGP(
                    sgp_object = SGP_object,
                    state = state.abbr,
                    steps = c("analyzeSGP", "combineSGP"),
                    sgp.config = config,
                    sgp.percentiles = TRUE,
                    sgp.projections = FALSE,
                    sgp.projections.lagged = FALSE,
                    sgp.percentiles.baseline = FALSE,
                    sgp.projections.baseline = FALSE,
                    sgp.projections.lagged.baseline = FALSE,
                    sgp.use.my.coefficient.matrices =
                        ifelse(is.null(coef_matrices), FALSE, TRUE), # Added
                    goodness.of.fit.print = FALSE,  #  Added
                    simulate.sgps = FALSE,
                    parallel.config = list(
                        BACKEND = "PARALLEL",
                        WORKERS = workers
                    )
                )
            # SGP_object@Data[
            #     YEAR == fyear,
            #     as.list(summary(SGP)),
            #     keyby = c("CONTENT_AREA", "GRADE")]

            # SGP_object <-
            #     abcSGP(
            #         sgp_object = resampd_data,
            #         state = state.abbr,
            #         steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
            #         sgp.config = config,
            #         sgp.percentiles = TRUE,
            #         sgp.projections = FALSE,
            #         sgp.projections.lagged = FALSE,
            #         sgp.percentiles.baseline = FALSE,
            #         sgp.projections.baseline = FALSE,
            #         sgp.projections.lagged.baseline = FALSE,
            #         sgp.use.my.coefficient.matrices = coef_matrices, # Added
            #         goodness.of.fit.print = FALSE,  #  Added
            #         simulate.sgps = FALSE,
            #         parallel.config = list(
            #             BACKEND = "PARALLEL",
            #             WORKERS = workers
            #         )
            #     )

            # Re-name and remove the SGP variables as necessary
            setnames(
                x = SGP_object@Data,
                old = "SGP",
                new = "SGP_Cnd_0_Boot"
            )

            rm.vars <-
                intersect(
                    names(SGP_object@Data),
                    c("SGP_ORDER_1", "SGP_ORDER", "SGP_LEVEL", "SGP_ORDER_2",
                      "SGP_NORM_GROUP", "SGP_NORM_GROUP_SCALE_SCORES",
                      "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED"
                    )
                )
            SGP_object@Data[, (rm.vars) := NULL]

            # CoefMatrices <- SGP_object@SGP[["Coefficient_Matrices"]]
            # save(CoefMatrices, file = paste0(output.dir, "/Boot_CoefMatrices.rda"))

            fname <-
                file.path(
                    output.dir,
                    paste0(
                        "School_Condition_0_", state.abbr, "_",
                        fyear, "_Resamp_", boot.n, ".csv"
                    )
                )
            fwrite(x = SGP_object@Data[YEAR == fyear], file = fname)
            zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")
            # Re-name and remove the SGP variables as necessary
            # rm(resampd_data)
            # sgp_data <- copy(SGP_object@Data)

            # setnames(
            #     x = sgp_data,
            #     old = "SGP",
            #     new = paste0("SGP_Boot_", boot.n)
            # )

            # rm.vars <-
            #     intersect(
            #         names(SGP_object@Data),
            #         c("SGP_ORDER_1", "SGP_ORDER", "SGP_LEVEL", "SGP_ORDER_2",
            #         "SGP_NORM_GROUP", "SGP_NORM_GROUP_SCALE_SCORES",
            #         "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED")
            #     )
            # sgp_data[, (rm.vars) := NULL]
        # }

        # setwd("..")
        # return(sgp_data)
        # fwrite(
        #     x = SGP_object@Data[YEAR == fyear],
        #     file = file.path(
        #         output.dir,
        #         paste0(
        #             "School_Condition_0_", state.abbr, "_",
        #             fyear, "_Resamp_", boot.n, ".csv"
        #         )
        #     )
        # )
        school_aggregation_all_students <-
            rbindlist(
            list(
                schoolAggrGator(
                data_table =
                    Georgia_Data_LONG[YEAR %in% c(2018, 2019) & GRADE %in% 3:8],
                growth.var = "SGP_Cnd_0"
                )[, Condition := "0"],
                lapply(
                    c("Race", "EconDis", "EL", "SWD"),
                    \(f) {
                        schoolAggrGator(
                        data_table =
                            Georgia_Data_LONG[YEAR == fyear & GRADE %in% 3:8, ],
                        growth.var = "SGP_Cnd_0",
                        groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
                        )[, Condition := "0"] |> setnames(f, "Group")
                    }
                    ) |> rbindlist()

        }
    }

resampleData =
    function(
        data,
        configs
    ){
        new_data <- data.table()
        tmp.key <- getKey(data)

        for (K in seq(configs)) {
            config.iter <- configs[[K]]
            for (G in seq(config.iter[["sgp.grade.sequences"]])) {
                cohort.iter <- config.iter
                cohort.iter[["sgp.grade.sequences"]] <-
                    cohort.iter[["sgp.grade.sequences"]][[G]]
                grade.length <- length(cohort.iter[["sgp.grade.sequences"]])
                cohort.lookup <-
                    data.table::SJ(
                        "VALID_CASE",
                        tail(cohort.iter[["sgp.content.areas"]], grade.length),
                        tail(cohort.iter[["sgp.panel.years"]], grade.length),
                        cohort.iter[["sgp.grade.sequences"]]
                    ) |>
                    data.table::setnames(tmp.key %w/o% "ID") |>
                        # order lookup table by years.
                        data.table::setkey(YEAR) |>
                        # avoid corrupt join in dcast.
                        data.table::setkey(NULL)

                for (var in tmp.key %w/o% "ID") {
                    mode(cohort.lookup[[var]]) <- mode(data[[var]])
                }
                data.table::setkeyv(data, tmp.key)

                tmp_long <- data[cohort.lookup]
                tmp.ids <-
                    tmp_long[
                        YEAR == tail(cohort.iter[["sgp.panel.years"]], 1),
                        ID
                    ]
                tmp_ids <-
                    data.table(
                        ID = sample(
                            x = tmp.ids,
                            size = length(tmp.ids),
                            replace = TRUE
                        ),
                        TMP_ID = uuid::UUIDgenerate(
                            n = length(tmp.ids),
                            output = "string"
                        ),
                        key = "ID"
                    )
                setkey(tmp_long, ID)
                tmp_long <- tmp_long[tmp_ids]
                tmp_long[, ID := TMP_ID][, TMP_ID := NULL]
                setkeyv(tmp_long, tmp.key)

                new_data <- rbindlist(list(new_data, tmp_long))
            }
        }
        new_data[]
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
