
calculate_SGPs =
    function(
        sgp_data, config, condition, state_abbr = "GA", workers
    ){
        # Create a folder "Condition_0" if it doesn't already exist
        folder_name <- paste0("./Condition_", condition)
        if (!dir.exists(folder_name)) dir.create(folder_name)

        # Calculate the SGPs using given config info
        setwd(folder_name)

        SGP_object <-
            abcSGP(
                sgp_object = sgp_data,
                state = state_abbr,
                steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
                sgp.config = config,
                sgp.percentiles = TRUE,
                sgp.projections = FALSE,
                sgp.projections.lagged = FALSE,
                sgp.percentiles.baseline = FALSE,
                sgp.projections.baseline = FALSE,
                sgp.projections.lagged.baseline = FALSE,
                simulate.sgps = FALSE,
                parallel.config = list(
                    BACKEND = "PARALLEL",
                    WORKERS = workers)
            )

        # remove all extraneous (pdf) versions of the model fit plots (created in the
        # "Goodness_of_Fit" directory at completion of the call to `abcSGP` above).
        all.files <-
            list.files("Goodness_of_Fit", recursive = TRUE, full.names = TRUE)
        file.remove(grep(".pdf|.Rdata", all.files, value = TRUE))
        unlink(grep("Decile_Tables", list.dirs(), value=TRUE), recursive = TRUE)

        # We will save the coefficient matrices from all separate analyses in case 
        # we need to replicate or compare results from this condition analysis.
        CoefMatrices <- SGP_object@SGP[["Coefficient_Matrices"]]
        save(CoefMatrices, file = paste0(folder_name, "_CoefMatrices.rda"))

        # Re-name and remove the SGP variables as necessary
        setnames(
            x = SGP_object@Data,
            old = "SGP",
            new = paste0("SGP_Cnd_", condition)
        )

        rm.vars <-
            intersect(
                names(SGP_object@Data),
                c("SGP_ORDER_1", "SGP_ORDER", "SGP_LEVEL", "SGP_ORDER_2",
                  "SGP_NORM_GROUP", "SGP_NORM_GROUP_SCALE_SCORES",
                  "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED")
            )
        SGP_object@Data[, (rm.vars) := NULL]

        SGP_object@SGP <- NULL

        setwd("..")
        return(SGP_object)
    }
