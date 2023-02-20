
bootstrapCond0 =
    function(
        sgp_data, config, state.abbr, state.name, fyear, workers,
        bootstrap.n = 100, coef_matrices = NULL, state_indicators,
        output.dir = "./Data/Phase_4-Metric_Bootstrap"
    ){
        # Ensure all states use Frisch-Newton estimator any time function is used
        SGPstateData[[state.abbr]][["SGP_Configuration"]][["rq.method"]] <- "fn"

        ##  Get original Cond 0, MinN 10 ratings table for Phase 4 comparisons step.
        refr_rates <-
            fread(
                file = paste0(
                "./Data/Phase_3-Accountability_Ratings/School_AcctRatings_Condition",
                "_0_Min10_", state.name, "_", fyear, "_AVI.csv"
                )
            )

        cond.comp.csi <-
        cond.comp.atsi <-
        acad.indc.dist <- data.table()

        for (boot.n in seq(bootstrap.n)) {
            started.boot <- proc.time()

        # Phase 2 - Growth Analyses
            SGP_object <-
                prepareSGP(
                    data = resampleData(sgp_data, config),
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
                        if (is.null(coef_matrices)) NULL else TRUE, # Added
                    goodness.of.fit.print = FALSE,  #  Added
                    simulate.sgps = FALSE,
                    parallel.config = list(
                        BACKEND = "PARALLEL",
                        WORKERS = workers
                    )
                )

            # Re-name and remove the SGP variables as necessary
            setnames(
                x = SGP_object@Data, # tmp_data,
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

        # Phase 2 - Results Aggregation
            tmp_aggregation <-
                rbindlist(
                    list(
                        schoolAggrGator(
                        data_table = SGP_object@Data[YEAR == fyear],
                        growth.var = "SGP_Cnd_0_Boot"
                        )[, Condition := "0"][, Group := "All"],
                        lapply(
                            c("Race", "EconDis", "EL", "SWD"),
                            \(f) {
                              schoolAggrGator(
                                data_table = SGP_object@Data[YEAR == fyear],
                                growth.var = "SGP_Cnd_0_Boot",
                                groups = c("SchoolID", "YEAR", "CONTENT_AREA", f)
                              )[, Condition := "0"] |> setnames(f, "Group")
                            }
                            ) |> rbindlist()
                    ),
                    use.names = TRUE
                )

            # Create a folder "Bootstrap" if it doesn't already exist
            if (!dir.exists(file.path(output.dir, "Phase_2"))) {
                dir.create(file.path(output.dir, "Phase_2"), recursive = TRUE)
            }
            fname <-
                file.path(
                    output.dir, "Phase_2",
                    paste0(
                        "School_Condition_0_", state.name, "_",
                        fyear, "_Resamp_", boot.n, ".csv"
                    )
                )
            fwrite(x = tmp_aggregation, file = fname)
            zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")

        # Phase 3 - Accountability Model
            focl_rates <-
                accountabilityModel(
                    condition = "0",
                    min.n = 10,
                    state_indicators = state_indicators,
                    cond_summary_table = tmp_aggregation
                )

            if (!dir.exists(file.path(output.dir, "Phase_3"))) {
                dir.create(file.path(output.dir, "Phase_3"))
            }
            fname <-
                file.path(
                    output.dir, "Phase_3",
                    paste0("School_AcctRatings_Condition_0_Min10_",
                           state.name, "_", fyear, "_Resamp_", boot.n, ".csv"
                    )
                )
            fwrite(x = focl_rates, file = fname)
            # School_AcctRatings_Condition_0_Min10_<State>_<FYear>_Resamp.csv
            zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")

        # Phase 4 - Comparison Metrics
        # 12 a.
            tmp_data <-
                merge(
                    refr_rates[Group == "All",
                        .(SchoolID, CSI, ACH_Z, Other_Z, SumScore)
                    ],
                    focl_rates[Group == "All",
                        .(SchoolID, CSI, ACH_Z, Other_Z, SumScore)
                    ],
                    by = "SchoolID", all = TRUE
                )
        # CSI - 13. & 14.
            n_tbl <- table(tmp_data[, CSI.x, CSI.y], exclude = NULL)
            p_tbl <- prop.table(n_tbl) |> round(3)

            cond.comp.csi <- rbindlist(list(
                cond.comp.csi,
                data.table(
                    Boot_Iteration = boot.n,
                    # paste0("Conditions 0 vs. ", cnd.nm, ", MinN = ", min.n),
                    Year = fyear,
                    N_Schools    = sum(n_tbl),
                    `%_Match`    = sum(diag(p_tbl)),
                    `%_FalsePos` = p_tbl[2, 1],
                    `%_FalseNeg` = p_tbl[1, 2],
                    `%_ExcludedPos` = ifelse(nrow(p_tbl) == 3, p_tbl[3, 2], NA),
                    `%_ExcludedNeg` = ifelse(nrow(p_tbl) == 3, p_tbl[3, 1], NA)
                )),
                fill = TRUE
            )

        # ATSI - 15. & 16.
            n_tbl <-
                focl_rates[Group != "All" & !is.na(ATSI),
                    .(ATSI_FOC = ifelse(any(ATSI == 1), 1, 0)),
                    keyby = "SchoolID"
                ][
                refr_rates[Group != "All" & !is.na(ATSI),
                    .(ATSI_REF = ifelse(any(ATSI == 1), 1, 0)),
                    keyby = "SchoolID"
                ]][, SchoolID := NULL] |>
                    table(exclude = NULL)
            p_tbl <- prop.table(n_tbl) |> round(3)

            cond.comp.atsi <- rbindlist(list(
                cond.comp.atsi,
                data.table(
                    Boot_Iteration = boot.n,
                    Year = fyear,
                    N_Schools    = sum(n_tbl),
                    `%_Match`    = sum(diag(p_tbl)),
                    `%_FalsePos` = p_tbl[2, 1],
                    `%_FalseNeg` = p_tbl[1, 2],
                    `%_ExcludedPos` = ifelse(nrow(p_tbl) == 3, p_tbl[3, 2], NA),
                    `%_ExcludedNeg` = ifelse(nrow(p_tbl) == 3, p_tbl[3, 1], NA)
                )),
                fill = TRUE
            )

        # 17. - 21. -- Distribution of Achievement and Other Indicator Scores
            acad.indc.dist <- rbindlist(list(
                acad.indc.dist,
                cbind(
                    data.table(
                        Boot_Iteration = boot.n,
                        Year = fyear
                    ),
                    tmp_data[,
                        .(ACH_Correl = cor(ACH_Z.x, ACH_Z.y, use = "complete.obs"),
                        ACH_RMSE =
                        sqrt(sum((ACH_Z.x - ACH_Z.y)^2, na.rm = TRUE) / sum(n_tbl)),
                        Other_Correl = cor(Other_Z.x, Other_Z.y, use = "complete.obs"),
                        Other_RMSE =
                        sqrt(sum((Other_Z.x - Other_Z.y)^2, na.rm = TRUE) / sum(n_tbl)),
                        SumScore_Correl = cor(SumScore.x, SumScore.y, use = "complete.obs"),
                        SumScore_RMSE =
                        sqrt(sum((SumScore.x - SumScore.y)^2, na.rm = TRUE) / sum(n_tbl))
                    )] |> round(3)
                )),
                fill = TRUE
            )

        #  End boot.n
            message(
                "\n\n\n\t\tCompleted Bootstap Iteration ", boot.n, " in ",
                SGP:::convertTime(SGP:::timetakenSGP(started.boot)), "\n\n"
            )
        }

        if (!dir.exists(file.path(output.dir, "Phase_4"))) {
                dir.create(file.path(output.dir, "Phase_4"))
        }

        fwrite(
            x = cond.comp.csi,
            file = file.path(
                output.dir, "Phase_4",
                paste0("Metric_Bootstrap_CSI_", fyear, ".csv")
            )
        )
        fwrite(
            x = cond.comp.atsi,
            file = file.path(
                output.dir, "Phase_4",
                paste0("Metric_Bootstrap_ATSI_", fyear, ".csv")
            )
        )
        fwrite(
            x = acad.indc.dist,
            file = file.path(
                output.dir, "Phase_4",
                paste0("Metric_Bootstrap_Ratings_Dist_", fyear, ".csv")
            )
        )

        Metric_Bootstrap <- list(
            CSI = cond.comp.csi,
            ATSI = cond.comp.atsi,
            DIST = acad.indc.dist
        )
        save(Metric_Bootstrap,
            file = file.path(
                output.dir, "Phase_4",
                paste0("Metric_Bootstrap_", fyear, ".rda")
            )
        )
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
