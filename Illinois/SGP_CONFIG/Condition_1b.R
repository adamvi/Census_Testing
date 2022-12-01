#####
###   2018 and 2019 configurations (Condition 1b)
#####

###   ELA

ELA_2018.config <- list(
    ELA.SKIP.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))
    ),
    ELA.2018 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2018"),
        sgp.grade.sequences = list(c("5", "6"))
    )
)

ELA_2019.config <- list(
    ELA.SKIP.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))
    ),
    ELA.2019 = list(
        sgp.content.areas = rep("ELA", 2),
        sgp.panel.years = c("2018", "2019"),
        sgp.grade.sequences = list(c("5", "6"))
    )
)

###   MATHEMATICS

MATHEMATICS_2018.config <- list(
    MATHEMATICS.SKIP.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2016", "2018"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))
    ),
    MATHEMATICS.2018 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2018"),
        sgp.grade.sequences = list(c("5", "6"))
    )
)

MATHEMATICS_2019.config <- list(
    MATHEMATICS.SKIP.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2017", "2019"),
        sgp.grade.sequences = list(c("3", "5"), c("6", "8"))
    ),
    MATHEMATICS.2019 = list(
        sgp.content.areas = rep("MATHEMATICS", 2),
        sgp.panel.years = c("2018", "2019"),
        sgp.grade.sequences = list(c("5", "6"))
    )
)
