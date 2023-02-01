# Authored by Emma Klugman, drawing heavily from work by Adam Van Iwaarden

# Student Growth Percentiles Analysis
#
# This section presents and explains the code used to conduct the Student
# Growth Percentiles (SGP) analyses. Each simulated testing condition is
# applied via the `R` code to the same set of data, thus only producing growth
# measures for the appropriate grades, content areas and years. At the end of
# each condition-specific analysis, the SGP variable is renamed to indicate
# the simulated condition before proceeding to the next SGP analysis step.
# Only cohort-referenced SGPs are calculated (SGP projections and targets are
# omitted). The goal of this step is simply to create growth percentiles and
# merge them into the longitudinal data before aggregation and investigation
# of the impact non-census testing has on school accountability measures. 

# Load relevant libraries
library(SGP)
library(tidyverse)
library(beepr)

# Set up parallelisation (highly recommended, since this script can take many hours to run!)
if (!exists("workers")) workers <- parallel::detectCores(logical = FALSE) #/2

# Read in the data
SGP_data = read_csv("Georgia - 1 - Cleaned and Merged Data/Student_LongTestData_Georgia_2016-2022_EK.csv")
# Retain only what we'll need to calculate SGPs for focus years 2018 and 2019
SGP_data = SGP_data[SGP_data$YEAR < 2020, ]

# SGP package expects a column "VALID_CASE" 
# I make all rows valid, since I excluded invalid columns already in Phase I code. 
SGP_data$VALID_CASE = "VALID_CASE"


#
# Set up some helper functions to reduce repeated code across the conditions:
#

calculate_simple_SGPs <- function(df, config_info, state_abbr = "GA", folder_name = "./Condition_0")
{
    # Create a folder "Condition_0" if it doesn't already exist
    if (!dir.exists(folder_name)) dir.create(folder_name)
    
    # Calculate the SGPs using given config info
    setwd(folder_name)
    
    # We use the `abcSGP` function from the `SGP` package to produce 2018 and 2019
    # student growth percentiles. We provide the function with the longitudinal
    # data that was previously cleaned and formatted, as well as the list of
    # analysis configurations and other relevant arguments to tailor the analyses
    # to our specifications.
    
    SGP_object <- abcSGP(sgp_object = df,
                         state = state_abbr,
                         steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
                         sgp.config = config_info,
                         sgp.percentiles = TRUE,
                         sgp.projections = FALSE,
                         sgp.projections.lagged = FALSE,
                         sgp.percentiles.baseline = FALSE,
                         sgp.projections.baseline = FALSE,
                         sgp.projections.lagged.baseline = FALSE,
                         simulate.sgps = FALSE,
                         parallel.config = list(
                            BACKEND = "PARALLEL",
                            WORKERS = workers))
    return(SGP_object)
}

tidy_up_SGPs <- function(returned_SGP_object, condition = "0")
{
    # Since we will only be using the growth results for creating reports, we
    # remove all extraneous (pdf) versions of the model fit plots (created in the
    # "Goodness_of_Fit" directory at completion of the call to `abcSGP` above).
    all.files <- list.files("Goodness_of_Fit", recursive = TRUE, full.names = TRUE)
    flrm.tf <- file.remove(grep(".pdf|.Rdata", all.files, value = TRUE))
    unlk.tf <- unlink(grep("Decile_Tables", list.dirs(), value=TRUE), recursive = TRUE)
    setwd("..")
    
    # We will save the coefficient matrices from all separate analyses in case 
    # we need to replicate or compare results from this condition analysis.
    CoefMatrices <- returned_SGP_object@SGP[["Coefficient_Matrices"]]
    save(CoefMatrices, file = paste("./Condition", condition, "CoefMatrices.rda", sep = "_"))
    
    #return(df)
    
    # Return just the column of computed SGPs
    return(returned_SGP_object@Data$SGP)
}

# ## Simulation Condition 0
# 
# In this simulation condition, we want to replicate the base condition of
# typical census-level testing with the base data set. Growth analyses will
# include grades 4 to 8, with consecutive-year assessment patterns. Students
# with a valid score from the previous year and grade level in their
# historical data will be included in the growth calculations and receive a
# SGP. Up to two prior scores will be used as available in the data.

ELA_2018.config <- list(ELA.2018 = list(
        sgp.content.areas = rep("ELA", 3),
        sgp.panel.years = c("2016", "2017", "2018"),
        sgp.grade.sequences = list(c("3", "4"), c("3", "4", "5"),
            c("4", "5", "6"), c("5", "6", "7"), c("6", "7", "8"))))
ELA_2019.config <- list(ELA.2019 = list(
        sgp.content.areas = rep("ELA", 3),
        sgp.panel.years = c("2017", "2018", "2019"),
        sgp.grade.sequences = list(c("3", "4"), c("3", "4", "5"),
            c("4", "5", "6"), c("5", "6", "7"), c("6", "7", "8"))))
MATHEMATICS_2018.config <- list(MATHEMATICS.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 3),
        sgp.panel.years = c("2016", "2017", "2018"),
        sgp.grade.sequences = list(c("3", "4"), c("3", "4", "5"),
            c("4", "5", "6"), c("5", "6", "7"), c("6", "7", "8"))))
MATHEMATICS_2019.config <- list(MATHEMATICS.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 3),
        sgp.panel.years = c("2017", "2018", "2019"),
        sgp.grade.sequences = list(c("3", "4"), c("3", "4", "5"),
            c("4", "5", "6"), c("5", "6", "7"), c("6", "7", "8"))))

# Combine the above into one config object, then clear out the constituent parts for tidiness. 
georgia_config_0 <- c(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)
rm(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)

# Calculate and save SGPs for Condition 0
SGP_cond_0 <- calculate_simple_SGPs(df = SGP_data, 
                                    config_info = georgia_config_0, 
                                    state_abbr = "GA", 
                                    folder_name = "./Condition_0")
# Extract the vector of SGPs from the returned object, add it as a column to our dataframe, tidy rest. 
SGP_data$SGP_Cnd_0 <- tidy_up_SGPs(SGP_cond_0, condition = "0")
beep()


#
# Note that condition 1a cannot use student-level growth scores, and instead calculates
# cohort-to-cohort improvement scores for schools. I mark these as NA. 
#
SGP_data$SGP_Cnd_1a <- NA


# ## Simulation Condition 1b
# 
# In this condition, students test twice per grade span (elementary and middle
# grades) in both subjects. Tests are administered every year in 3rd, 5th, 6th
# and 8th grades. Subsequently, all growth analyses will use a single prior
# score, and can be done with a either consecutive- or skipped-year assessment
# patterns. Unlike the other simulation conditions, 1b requires *both* 
# consecutive- and skipped-year configuration scripts.

ELA_2018.config <- list(
    ELA.SKIP.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))),
    ELA.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2018"),
        sgp.grade.sequences = list(c("5", "6"))))
ELA_2019.config <- list(
    ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))),
    ELA.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2018", "2019"),
        sgp.grade.sequences = list(c("5", "6"))))
MATHEMATICS_2018.config <- list(
    MATHEMATICS.SKIP.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))),
    MATHEMATICS.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2018"),
        sgp.grade.sequences = list(c("5", "6"))))
MATHEMATICS_2019.config <- list(
    MATHEMATICS.SKIP.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))),
    MATHEMATICS.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2018", "2019"),
        sgp.grade.sequences = list(c("5", "6"))))

# Combine the above into one config object, then clear out the constituent parts for tidiness. 
georgia_config_1b <- c(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)
rm(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)

# Calculate and save SGPs for Condition 1b
SGP_cond_1b <- calculate_simple_SGPs(df = SGP_data, 
                                     config_info = georgia_config_1b, 
                                     state_abbr = "GA", 
                                     folder_name = "./Condition_1b")
# Extract the vector of SGPs from the returned object, add it as a column to our dataframe, tidy rest. 
SGP_data$SGP_Cnd_1b <- tidy_up_SGPs(SGP_cond_1b, condition = "1b")
beep()


# ## Simulation Condition 1c
# 
# In this condition, students alternate testing in each subject across grade
# levels. In this simulation, students in grades 3, 5, and 7 take ELA and
# students in grades 4, 6, 8 take mathematics each year. As with condition 1b,
# all growth analyses will be conditioned on a single prior score, but only
# skipped-year assessment patterns can be analyzed.

# Note that this particular testing pattern means traditional elementary schools 
# will only have growth measures for grade 5 ELA, while traditional middle schools 
# will have growth indicators in all three grades and both content areas. 
# The only contribution mathematics makes to an elementary school's accountability 
# calculation is through grade 4 proficiency (status).

ELA_2018.config <- list(ELA.SKIP.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary Grades
            c("5", "7")  # Middle Grades
            )))
ELA_2019.config <- list(ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary Grades
            c("5", "7")  # Middle Grades
            )))
MATHEMATICS_2018.config <- list(MATHEMATICS.SKIP.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("4", "6"), c("6", "8") # Middle ONLY
            )))
MATHEMATICS_2019.config <- list(MATHEMATICS.SKIP.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("4", "6"), c("6", "8") # Middle ONLY
            )))

# Combine the above into one config object, then clear out the constituent parts for tidiness. 
georgia_config_1c <- c(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)
rm(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)

# Calculate and save SGPs for Condition 1b
SGP_cond_1c <- calculate_simple_SGPs(df = SGP_data, 
                                     config_info = georgia_config_1c, 
                                     state_abbr = "GA", 
                                     folder_name = "./Condition_1c")
# Extract the vector of SGPs from the returned object, add it as a column to our dataframe, tidy rest. 
SGP_data$SGP_Cnd_1c <- tidy_up_SGPs(SGP_cond_1c, condition = "1c")
beep()


# ## Simulation Condition 2
# 
# In this condition, all students are tested every two years in each grade and
# subject on the state's assessments. There are two instances of this
# condition to simulate:
# 
# * Testing only occurs in even years - (e.g., 2016, 2018, etc.)
# * Testing only occurs in even years - (e.g., 2017, 2019, etc.) 
#
# In both instances, in a year that testing occurs, all students are tested in
# every grade and subject. As with condition 1c, all growth analyses will be
# conditioned on a single prior score with skipped-year patterns.

# We calculate SGPs here for both the "even" condition (calculable for 2018)
# and the "odd" condition (calculable for 2019), and will analyse them 
# separately in a later step. 

ELA_2018.config <- list(ELA.SKIP.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )))
ELA_2019.config <- list(ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )))
MATHEMATICS_2018.config <- list(MATHEMATICS.SKIP.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )))
MATHEMATICS_2019.config <- list(MATHEMATICS.SKIP.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )))

# Combine the above into one config object, then clear out the constituent parts for tidiness. 
georgia_config_2 <- c(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)
rm(ELA_2019.config, MATHEMATICS_2019.config, ELA_2018.config, MATHEMATICS_2018.config)

# Calculate and save SGPs for Condition 2
SGP_cond_2 <- calculate_simple_SGPs(df = SGP_data, 
                                    config_info = georgia_config_2, 
                                    state_abbr = "GA", 
                                    folder_name = "./Condition_2")
# Extract the vector of SGPs from the returned object, add it as a column to our dataframe, tidy rest. 
SGP_data$SGP_Cnd_2 <- tidy_up_SGPs(SGP_cond_2, condition = "2")
beep()


# ## Simulation Condition 3
# 
# In this condition, students in specific grades are tested every two years. 
# As with Condition 2, there are two instances of this condition to simulate:
# 
# * Testing only occurs in even years - (e.g., 2016, 2018, etc.)
# * Testing only occurs in even years - (e.g., 2017, 2019, etc.) 
#
# As in condition 1b, only students in grades 3, 5, 6, and 8 are tested. 
# When students are tested, they are tested in both ELA and Math. 
# 
# Since testing occurs only every other year, all growth analyses will be 
# conditioned on a single prior score with skipped-year patterns.
# 
# Separate analyses for even and odd years will be conducted later. 


# Here, we will simply copy the results from condition 1b to a new variable
# for condition 3. The grade 6 SGPs, which were consecutive-year (grade 5 to
# grade 6) will be omitted, as every-other-year testing in condition 3 produces 
# no consecutive scores.  
SGP_data <- SGP_data %>% 
    mutate(SGP_Cnd_3 = ifelse(GRADE %in% c(5, 8),
                              SGP_Cnd_1b,
                              NA))


# Save data
SGP_data <- SGP_data %>% 
    select(-VALID_CASE) %>% 
    write_csv(file = 
                  "Georgia - 2 - Simulated Study Conditions/Student_LongTestData_Georgia_2016-2019_EK_SGPs.csv")




# Abandoned extra code: (from within tidy_up_SGPs fn)
    # Re-name and remove the SGP variables as necessary:
    # In order to keep all growth results in the same longitudinal dataset, we
    # will add a `Cnd_n` tag to growth related variables of interest. Extraneous
    # variables will be removed as well before moving on to the next simulation
    # condition. 
    # df <- returned_SGP_object@Data %>% 
    #     # new_name = old_name syntax
    #     rename_with(.fn = ~paste("SGP_Cnd", condition, sep = "_"), 
    #                 .cols = SGP) %>% 
    #     # drop extraneous columns
    #     select(-c("SGP_NORM_GROUP", "SGP_NORM_GROUP_SCALE_SCORES",
    #               "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED",
    #               "SGP_LEVEL", starts_with("SGP_ORDER")))