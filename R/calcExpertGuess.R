#' Calculate expert guesses
#'
#' @author Falk Benke
#'
#' @param subtype must be 'tradeConstraints' (more to come)
#' @export
calcExpertGuess <- function(subtype) {
  if (subtype != "tradeConstraints") {
    stop("Invalid subtype. Supported subtypes: 'tradeConstraints'")
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

  return(list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description,
    isocountries = isocountries
  ))
}
