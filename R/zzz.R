.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste0("\nPlease cite the 'OPSR' package as:\n",
           "Heimgartner, D. and X. Wang (2024) ",
           "OPSR: A package for estimating ordinal probit switching regression models in R. tbc.\n\n",
           "Wang, X. and P. L. Mokhtarian (2024) ",
           "Examining the treatment effect of teleworking on vehicle-miles driven: Applying an ordered probit selection model and incorporating the role of travel stress, ",
           "Transportation Research Part A, 186, 104072, ",
           "doi:10.1016/j.tra.2024.104072.\n\n",
           "If you have questions, suggestions, or comments regarding the 'OPSR' ",
           "package, please open an issue on https://github.com/dheimgartner/OPSR\n\n",
           "To see these entries in BibTeX format, use 'citation('OPSR')'"),
    domain = NULL,  appendLF = TRUE)
}
