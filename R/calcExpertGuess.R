#' Calculate expert guesses
#'
#' @author Falk Benke
#'
#' @param subtype must be 'tradeConstraints' (more to come)
#' @export
calcExpertGuess <- function(subtype) {
  if (! subtype %in% c("tradeConstraints","biocharPrices")) {
    stop("Invalid subtype. Supported subtypes: 'tradeConstraints' and 'biocharPrices'")
  }

  x <- readSource("ExpertGuess", subtype = subtype, convert = FALSE)

  if (subtype == "tradeConstraints") {
    unit <- "unitless"
    description <- c(
      "parameter by Nicolas Bauer (2024) for the region specific ",
      "trade constraints, values different to 1 activate constraints ",
      "and the value is used as effectiveness to varying degrees ",
      "such as percentage numbers"
    )
    isocountries <- FALSE
  }

  if (subtype == "biocharPrices"){
    unit <- "USD 2015/t biochar"
    description <- c("Biochar price assumptions over time.
    Assumptions based on collection of current bulk sale prices
    (see Dorndorf et al (submitted)).")
    isocountries <- FALSE
  }

  return(list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    isocountries = isocountries
  ))
}
