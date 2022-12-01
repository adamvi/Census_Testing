#####
###   2018 and 2019 configurations (Condition 1c)
#####

###   ELA

ELA_2018.config <- list(
    ELA.SKIP.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("3", "5"), # Elementary Grades
            c("5", "7")  # Middle Grades
        )
    )
)

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

###   MATHEMATICS

MATHEMATICS_2018.config <- list(
    MATHEMATICS.SKIP.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(
            c("4", "6"), c("6", "8") # Middle ONLY
        )
    )
)

MATHEMATICS_2019.config <- list(
    MATHEMATICS.SKIP.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(
            c("4", "6"), c("6", "8") # Middle ONLY
        )
    )
)
