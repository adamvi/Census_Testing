#+ cond-0, include = FALSE, purl = FALSE
###############################################################################
####                                                                       ####
####        Create 2018 and 2019 SGPs for State A - All Conditions         ####
####                                                                       ####
###############################################################################

#' # Student Growth Percentiles Analysis
#'
#' This section presents and explains the code used to conduct the Student
#' Growth Percentiles (SGP) analyses. Each simulated testing condition is
#' applied, via `R` code configurations, to the same set of data, thus only
#' producing growth measures for students with valid records in the appropriate
#' grades, content areas and years. At the end of each condition-specific
#' analysis, the SGP variable is renamed to indicate the simulated condition
#' before proceeding to the next condition-specific growth analysis step.
#' Only cohort-referenced SGPs are calculated (growth projections and targets
#' are omitted). The goal of this step is simply to create growth percentiles
#' and merge them into a single longitudinal dataset before aggregation and
#' investigation of the impact non-census testing has on school accountability
#' measures.
#'
#' The code presented below is provided as an example of how the growth
#' calculations are conducted for all states in the study. The student data for
#' each state will be cleaned and formatted according to the study guidelines,
#' as was done with the exemplar data from the `SGP` package (@sgp) in the
#' example Phase 1 code in the previous section.
#'
#' ## Load SGP package and modify `SGPstateData`
#'
#' The `SGP` package is required for all growth percentile analyses.
#'
#+ sgp-calc-pkg, echo = TRUE, purl = TRUE
require(SGP)
require(data.table)
source("../functions/calculate_SGPs.R")

#' We will use the assessment meta-data from the "Demonstration_COVID"
#' (abbreviated "DEMO_COVID") dataset stored in the `SGPstateData` object.
#' This meta-data is required to use various functions in the `SGP` package.
#'
#+ sgp-calc-meta, echo = TRUE, purl = TRUE
SGPstateData[["State_A"]] <- SGPstateData[["DEMO_COVID"]]

#+ sgp-calc-meta-misc, echo = FALSE, purl = TRUE
SGPstateData[["State_A"]][["Growth"]][["Levels"]] <-
  SGPstateData[["State_A"]][["Growth"]][["Cutscores"]] <-
    SGPstateData[["State_A"]][["SGP_Configuration"]][["percentile.cuts"]] <-
      NULL


#' # Condition-Specific Growth Analyses
#'
#' 
#+ sgp-calc-data, echo = FALSE, purl = TRUE
#  Load cleaned, merged and formatted data
if (!exists("State_A_Data_LONG")) {
  source("../functions/freadZIP.R")
  State_A_Data_LONG <-
    freadZIP(
      "Data/Cleaned_Data/Student_LongTestData_State_A_2016-2022_AVI.csv.zip"
    )[YEAR < 2020]
} else {
  State_A_Data_LONG <- State_A_Data_LONG[YEAR < 2020]
}

#' ## Simulation Condition 0
#' 
#' In this simulation condition, we want to replicate the base condition of
#' typical census-level testing with the base data set. Growth analyses will
#' include grades 4 to 8, with consecutive-year assessment patterns. Students
#' with a valid score from the previous year and grade level in their
#' historical data will be included in the growth calculations and receive a
#' SGP. Up to two prior scores will be used as available in the data.
#' 
#' ### Load and combine SGP config scripts
#'
#' The growth calculation functions of the `SGP` software package allow users
#' to manually specify which test progressions to run. That is, we can define
#' the unique **year-by-grade-by-content area** cohorts of students included in
#' each analysis.
#'
#' As an example, the 2019 ELA analyses/cohorts are specified with this code:
#'
#+ cond-0-config-ex, echo = TRUE, purl = TRUE
ELA_2019.config <- list(
    ELA.2019 = list(
        sgp.content.areas = rep("ELA", 3),
        sgp.panel.years = c("2017", "2018", "2019"),
        sgp.grade.sequences = list(
            c("3", "4"), c("3", "4", "5"), # Elementary Grades
            c("4", "5", "6"), c("5", "6", "7"), c("6", "7", "8") # Middle
        )
    )
)

#' All configurations are housed in condition specific `R` code scripts. Here
#' we read these in and combine them into a single list object, `state.a.config`,
#' that will be supplied to the `abcSGP` function.
#' 
#+ cond-0-config, echo = TRUE, purl = TRUE
source("SGP_CONFIG/Condition_0.R")

state.a.config <-
    c(ELA_2019.config,
      MATHEMATICS_2019.config,
      ELA_2018.config,
      MATHEMATICS_2018.config
    )

#' ### Calculate condition 0 SGPs
#'
#' We use the `abcSGP` function from the `SGP` package to produce 2018 and 2019
#' student growth percentiles. We provide the function with the longitudinal
#' data that was previously cleaned and formatted, as well as the list of
#' analysis configurations and other relevant arguments to tailor the analyses
#' to our specifications.
#'
#' The SGP analysis section of the appendix assumes the user is operating with
#' their working directory set to "*./Condition_0*".

#+ cond-0-wd1, echo = TRUE, purl = TRUE

#+ cond-0-abcsgp, echo = TRUE, message = FALSE, purl = TRUE
setwd("./Condition_0")
State_A_SGP <-
    abcSGP(
        sgp_object = State_A_Data_LONG,
        state = "State_A",
        steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
        sgp.config = state.a.config,
        sgp.percentiles = TRUE,
        sgp.projections = FALSE,
        sgp.projections.lagged = FALSE,
        sgp.percentiles.baseline = FALSE,
        sgp.projections.baseline = FALSE,
        sgp.projections.lagged.baseline = FALSE,
        simulate.sgps = FALSE,
        parallel.config = list(
            BACKEND = "PARALLEL",
            WORKERS = workers
        )
    )
setwd("..")

#+ cond-0-trimrepo, echo = FALSE, message = FALSE, purl = TRUE
# Since we will only be using the growth results for creating reports, we
# remove all extraneous (pdf) versions of the model fit plots (created in the
# "Goodness_of_Fit" directory at completion of the call to `abcSGP` above).
setwd("./Condition_0")
all.files <-
  list.files("Goodness_of_Fit", recursive = TRUE, full.names = TRUE)
flrm.tf <-
  file.remove(grep(".pdf|.Rdata", all.files, value = TRUE))
unlk.tf <-
  unlink(grep("Decile_Tables", list.dirs(), value=TRUE), recursive = TRUE)
setwd("..")

#' ###  Re-name and remove the SGP variables as necessary
#' 
#' In order to keep all growth results in the same longitudinal dataset, we
#' will add a `Cnd_0` tag to growth related variables of interest. Extraneous
#' variables will be removed as well before moving on to the next simulation
#' condition. We will save the coefficient matrices from all separate analyses
#' in case we need to replicate or compare results from this condition analysis.
#' 
#+ cond-0-cleanup, echo = TRUE, message = FALSE, purl = TRUE
setnames(x = State_A_SGP@Data, old = "SGP", new = "SGP_Cnd_0")
State_A_SGP@Data[,
    c("SGP_NORM_GROUP", "SGP_NORM_GROUP_SCALE_SCORES",
      "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED"
    ) := NULL
]

Condition_0_CoefMatrices <- copy(State_A_SGP@SGP[["Coefficient_Matrices"]])
save(Condition_0_CoefMatrices,
     file = "./Condition_0/Condition_0_CoefMatrices.rda")
State_A_SGP@SGP <- NULL

#+ cond-1b, include = FALSE, purl = FALSE
#####
##     Condition 1b
#####
if (!dir.exists("./Condition_1b")) dir.create("./Condition_1b")

#' ## Simulation Condition 1b
#' 
#' In this condition, students test twice per grade span (elementary and middle
#' grades) in both subjects. Tests are administered every year in 3rd, 5th, 6th
#' and 8th grades. Subsequently, all growth analyses will use a single prior
#' score, and can be done with a either consecutive- or skipped-year assessment
#' patterns.
#' 
#' ### Load and combine SGP config scripts
#'
#' In order to avoid errors in specification of our analysis configurations,
#' we first remove all previous configuration related objects before reading
#' in the code for condition 1b and proceeding as before. Unlike the other
#' simulation conditions, 1b requires *both* consecutive- and skipped-year
#' configuration scripts.
#' 
#' The 2019 ELA configuration code is provided here as an example and for
#' comparison with the code provided above for condition 0:
#'
#+ cond-1b-config-ex, echo = TRUE, purl = TRUE, eval = FALSE
ELA_2019.config <- list(
    ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), #  Elementary Grades
            c("6", "8")  #  Middle Grades
        )
    ),
    ELA.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2018", "2019"),
        sgp.grade.sequences = list(c("5", "6")) # Middle Only
    )
)

#+ cond-1b-config, echo = TRUE, purl = TRUE
rm(list = grep(".config", ls(), value = TRUE))
source("SGP_CONFIG/Condition_1b.R")

sgp_config.c1b <-
    c(ELA_2019.config,
      MATHEMATICS_2019.config,
      ELA_2018.config,
      MATHEMATICS_2018.config
    )

#' ### Calculate condition 1b SGPs
#'
#' We again use the `abcSGP` function to compute the student growth percentiles
#' for this simulation condition. Here we use the data with results from
#' condition 0. The updated list of analysis configurations is now provided,
#' and all other relevant arguments remain the same.
#'
#+ cond-1b-abcsgp, echo = TRUE, message = FALSE, purl = TRUE
State_A_SGP <-
    calculate_SGPs(
      sgp_data = State_A_SGP,
      config = sgp_config.c1b,
      condition = "1b",
      workers = list(TAUS = 15)
    )


#+ cond-1c, include = FALSE, purl = FALSE
#####
##     Condition 1c
#####

#' ## Simulation Condition 1c
#' 
#' In this condition, students alternate testing in each subject across grade
#' levels. In this simulation, students in grades 3, 5, and 7 take ELA and
#' students in grades 4, 6, 7 take mathematics each year. As with condition 1b,
#' all growth analyses will be conditioned on a single prior score, but only
#' skipped-year assessment patterns can be analyzed.
#'
#' ### Load and combine SGP config scripts
#'
#' We again remove all previous configuration related objects before reading
#' in the condition 1c course progression code. The 2019 ELA configurations are
#' once again provided here for comparison with other simulation conditions.
#'
#+ cond-1c-config-ex, echo = TRUE, purl = TRUE, eval = FALSE
ELA_2019.config <- list(
    ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary Grades
            c("5", "7")  # Middle Grades
        )
    )
)

#' The mathematics configurations are nearly identical to the ELA code, with
#' the exception of the `sgp.grade.sequences` element, which specifies the
#' grades 4 to 6 and grades 6 to 8 progressions. Note that this particular
#' testing pattern means traditional elementary schools will only have growth
#' measures for grade 5 ELA, while traditional middle schools will have growth
#' indicators in all three grades and both content areas. The only contribution
#' mathematics makes to a school's accountability calculation is through grade
#' 4 proficiency (status).
#'
#+ cond-1c-config, echo = TRUE, purl = TRUE
rm(list = grep(".config", ls(), value = TRUE))
source("SGP_CONFIG/Condition_1c.R")

sgp_config.c1c <-
    c(ELA_2019.config,
      MATHEMATICS_2019.config,
      ELA_2018.config,
      MATHEMATICS_2018.config
    )

#' ### Calculate condition 1c SGPs
#'
#' The call to the`calculate_SGPs` function here performs the same calculation
#' tasks as those made for conditions 1b. The `config` list specifies the
#' cohort data to use in the growth analyses, and the `condition` argument will
#' be used to create and change directories in which the analyses are run, and
#' rename the SGP variable for this condition calculations.
#'
#+ cond-1c-abcsgp, echo = TRUE, message = FALSE, purl = TRUE
State_A_SGP <-
    calculate_SGPs(
      sgp_data = State_A_SGP,
      config = sgp_config.c1c,
      condition = "1c",
      workers = list(TAUS = 15)
    )

#+ cond-2, include = FALSE, purl = FALSE
#####
##     Condition 2
#####

#' ## Simulation Condition 2
#' 
#' In this condition, all students are tested every two years in each grade and
#' subject on the state's assessments. There are two instances of this
#' condition to simulate:
#' 
#' * Testing only occurs in even years - (e.g., 2016, 2018, etc.)
#' * Testing only occurs in even years - (e.g., 2017, 2019, etc.) 
#'
#' In both instances, in a year that testing occurs, all students are tested in
#' every grade and subject. As with condition 1c, all growth analyses will be
#' conditioned on a single prior score with skipped-year patterns.
#'
#' ### Load and combine SGP config scripts
#'
#' We again remove all previous configuration related objects before reading
#' in the condition 2 course progression code. The 2019 ELA configurations are
#' once again provided here for comparison with other simulation conditions.
#'
#+ cond-2-config-ex, echo = TRUE, purl = TRUE, eval = FALSE
ELA_2019.config <- list(
    ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary Grades
            c("4", "6"), c("5", "7"), c("6", "8") # Middle Grades
        )
    )
)

#+ cond-2-config, echo = TRUE, purl = TRUE
rm(list = grep(".config", ls(), value = TRUE))
source("SGP_CONFIG/Condition_2.R")

sgp_config.c2 <-
    c(ELA_2019.config,
      MATHEMATICS_2019.config,
      ELA_2018.config,
      MATHEMATICS_2018.config
    )

#' ### Calculate condition 2 SGPs
#'
#' The call to the`calculate_SGPs` function here performs the same calculation
#' tasks as those made for conditions 1b and 1c. The `config` list specifies the
#' cohort data to analyze for condition 2, and the data object `State_A_SGP@Data`
#' will now include the results from conditions 0 through 2 when the calculations
#' are complete.
#'
#+ cond-2-abcsgp, echo = TRUE, message = FALSE, purl = TRUE
State_A_SGP <-
    calculate_SGPs(
      sgp_data = State_A_SGP,
      config = sgp_config.c2,
      condition = "2",
      workers = list(TAUS = 15)
    )


#+ cond-3, include = FALSE, purl = FALSE
#####
##     Condition 3
#####
if (!dir.exists("./Data")) dir.create("./Data")

#' ## Simulation Condition 3
#' 
#' In this condition, all students are tested every two years at specific grade
#' and subject on the state’s assessments. As with Condition 2, there are two
#' instances of this condition to simulate:
#' 
#' * Testing only occurs in even years - (e.g., 2016, 2018, etc.)
#' * Testing only occurs in even years - (e.g., 2017, 2019, etc.) 
#'
#' In both instances, when testing occurs, students are tested specific grades
#' in both subject areas. As with condition 1c, all growth analyses will be
#' conditioned on a single prior score with skipped-year patterns.

#' ### SGP config scripts
#'
#' The pattern of testing for this condition is identical to that of condition
#' 1b, with the exception of skipping years. This means that we have already
#' calculated the SGPs for these patterns and do not need to reanalyze the
#' data to get these results. Instead we can simply copy the results from
#' simulation condition 1b that use the skipped-year progressions (i.e. results
#' for grades 5 and 8, but not 6th grade).
#' 
#' For the sake of completeness, however, the 2019 ELA configurations for this
#' condition would be a subset of the condition 1b code, such as this:
#'
#+ cond-3-config-ex, echo = TRUE, purl = TRUE, eval = FALSE
ELA_2019.config <- list(
    ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), #  Elementary Grades
            c("6", "8")  #  Middle Grades
        )
    )
)

#' ### Use condition 1b growth for condition 3
#'
#' Here we will simply copy the results from condition 1b to a new variable
#' for condition 3. The grade 6 SGPs, which were consecutive-year (grade 5 to
#' grade 6) will be omitted.
#'
#+ cond-3-sgp-copy, echo = TRUE, message = FALSE, purl = TRUE
State_A_SGP@Data[
    GRADE %in% c(5, 8),
    SGP_Cnd_3 := SGP_Cnd_1b
]


#' ##  Save data
#'
#+ data-analysis-save, echo = TRUE, purl = TRUE
if (!dir.exists("Data/Student_Growth"))
    dir.create("Data/Student_Growth", recursive = TRUE)

State_A_Data_LONG <- copy(State_A_SGP@Data)
save(State_A_Data_LONG, file = "Data/Student_Growth/State_A_Data_LONG.rda")

fname <- "Data/Student_Growth/Student_LongTestData_State_A_2016-2019_AVI.csv"
fwrite(State_A_SGP@Data, file = fname)
zip(zipfile = paste0(fname, ".zip"), files = fname, flags = "-mqj")
