#' Provide price path data
#'
#' @author Tabea Dorndorf
#'
calcBiocharPricePath <- function() {
  x <- readSource("BiocharData", subtype = "biocharPrices")

  return(list(
    x = x,
    weight = NULL,
    unit = "USD 2015/t biochar",
    description = "Biochar price assumptions over time.
    Assumptions based on collection of current bulk sale prices
    (see Dorndorf et al (submitted)).",
    isocountries = FALSE
  ))
}
