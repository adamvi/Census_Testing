#####
###   2018 and 2019 configurations (Condition 2)
#####

###   ELA

ELA_2018.config <- list(
    ELA.SKIP.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )
    )
)

ELA_2019.config <- list(
    ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )
    )
)

###   MATHEMATICS

MATHEMATICS_2018.config <- list(
    MATHEMATICS.SKIP.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )
    )
)

MATHEMATICS_2019.config <- list(
    MATHEMATICS.SKIP.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary
            c("4", "6"), c("5", "7"), c("6", "8") # Middle
        )
    )
)
