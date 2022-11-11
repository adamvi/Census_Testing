###############################################################################
####                                                                       ####
####   Example code for producing reports (draft and stylized) - State A   ####
####                                                                       ####
###############################################################################


###  Required Packages for this report:
# remotes::install_github("Rdatatable/data.table")
# remotes::install_github("centerforassessment/SGP")
# remotes::install_github("centerforassessment/cfaTools")
# remotes::install_github("centerforassessment/cfaDocs")


##  Draft report format(s) - "Working Paper" and MS Word .docx
params <- list(format = "draft")
rmarkdown::render(
    input = "./report/State_A_Growth_Calculation_Report.Rmd",
    output_dir = file.path(getwd(), "report"),
    output_file = "State_A_Growth_Calculation_DRAFT.html",
    output_format = cfaDocs::w_paper_paged(),
    knit_root_dir = file.path(getwd(), "State_A"),
    clean = FALSE # Save for rendering the .docx draft version 
)
pagedown::chrome_print(input = "./report/State_A_Growth_Calculation_DRAFT.html")

##    MS Word version of the DRAFT/working paper
file.rename(
    from = "./report/State_A_Growth_Calculation_Report.knit.md",
    to   = "./report/State_A_Growth_Calculation_DRAFT.md"
)
rmarkdown::render(
    input = "./report/State_A_Growth_Calculation_DRAFT.md",
    output_dir = file.path(getwd(), "report"),
    output_format = "word_document"
)


###   CFA Stylized Themes

#  The output formats with the CFA stylized themes can use either 1) the
#  `knit`ed file from above (like the .docx version), or 2) by re-rendering
#  the report. Makes a difference here because the "State A" document is
#  set up to evaluate the code. This probably isn't a good idea with full
#  sized states...


##  Appendix to final report

#   Option 1)
rmarkdown::render(
    input = "./report/State_A_Growth_Calculation_DRAFT.md",
    output_dir = file.path(getwd(), "report"),
    output_file = "State_A_Growth_Calculation_APPENDIX.html",
    output_format = cfaDocs::cfa_paged(template = "appendix")
)

#   Option 2)
# params <- list(format = "appendix") # , appendix_prefix = "B"
# rmarkdown::render(
#     input = "./report/State_A_Growth_Calculation_Report.Rmd",
#     output_dir = file.path(getwd(), "report"),
#     output_file = "State_A_Growth_Calculation_APPENDIX.html",
#     output_format = cfaDocs::cfa_paged(template = "appendix"),
#     knit_root_dir = file.path(getwd(), "State_A"),
#     clean = FALSE # Ehh why not
# )

pagedown::chrome_print(input = "./report/State_A_Growth_Calculation_APPENDIX.html")


##  FINAL (CFA stylized theme) REPORT

#   Option 1) - NOTE! - no inside cover, executive summary, etc.
# rmarkdown::render(
#     input = "./report/State_A_Growth_Calculation_DRAFT.md",
#     output_dir = file.path(getwd(), "report"),
#     output_file = "State_A_Growth_Calculation_Report.html",
#     output_format = cfaDocs::cfa_paged()
# )

#   Option 2)
params <- list(format = "final")
inside_cover <-
    list(
        client_city = "Anywhere",
        client_state = "U.S.A.",
        client_organization = "The Gates-Walton Foundations",
        client_name = "Bill Gates and the Walton Family",
        acknowledgements =
        "We want to thank the State A Department of Education for their
            help in obtaining the data for this report, the
            **Gates-Walton Foundations** for their ongoing support and the
            Walmart greeter who told us to never (EVER) use Windows OS."
    )

rmarkdown::render(
    input = "./report/State_A_Growth_Calculation_Report.Rmd",
    output_dir = file.path(getwd(), "report"),
    output_format = cfaDocs::cfa_paged(),
    knit_root_dir = file.path(getwd(), "State_A"),
    clean = FALSE # Ehh why not
)
res <- readLines("./report/State_A_Growth_Calculation_Report.html")
writeLines(
    text = gsub(" ¶ ", "", res),
    con = "./report/State_A_Growth_Calculation_Report.html"
)
pagedown::chrome_print(input = "./report/State_A_Growth_Calculation_Report.html")

##  Remove the intermediate HTML files after PDFs are created
file.remove(
    c("./report/State_A_Growth_Calculation_DRAFT.html",
      "./report/State_A_Growth_Calculation_DRAFT.md",
      "./report/State_A_Growth_Calculation_APPENDIX.html",
      "./report/State_A_Growth_Calculation_Report.html",
      "./report/State_A_Growth_Calculation_Report.knit.md"
    )
)
