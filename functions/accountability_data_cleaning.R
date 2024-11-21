tidyGroupNames = function(df){
    dplyr::mutate(
        .data = df,
        Group =
            fct_recode(
                Group,
                "Native" = "American Indian/Alaskan", 
                "Native" = "American Indian/Alaskan Native", 
                "Multiracial" = "Multi-Racial",
                "SWD" = "Students with Disability",
                "SWD" = "Students With Disability", 
                "EL" = "English Learners", 
                "EconDis" = "Economically Disadvantaged",
                "All" = "ALL Students",
                "Asian" = "Asian/Pacific Islander"
            )
    )
}

