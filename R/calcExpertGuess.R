#' Calculate expert guesses
#'
#' @author Falk Benke
#'
#' @param subtype must be one of `biocharPrices`, `gridFactor`, `tradeConstraints`
calcExpertGuess <- function(subtype) {

  subtypes <- c(
    "biocharPrices",
    "gridFactor",
    "tradeConstraints"
  )

  if (!(subtype %in% subtypes)) {
    stop("Invalid subtype. Supported subtypes: ", paste0(subtypes, collapse = ", "))
  }

  isocountries <- c(
    "biocharPrices" = FALSE,
    "gridFactor" = TRUE,
    "tradeConstraints" = FALSE
  )

  x <- readSource("ExpertGuess", subtype = subtype, convert = isocountries[[subtype]])

  if (subtype == "biocharPrices") {
    unit <- "USD 2015/t biochar"
    description <- glue::glue(
      "Biochar price assumptions over time. Assumptions based on collection of \\
      current bulk sale prices (see Dorndorf et al (submitted))."
    )
    weight <- NULL
  } else if (subtype == "gridFactor") {
    unit <- "factor"
    getNames(x) <- NULL
    description <- glue::glue(
      "multiplicative factor that scales total grid requirements \\
      down in comparatively small or homogeneous regions"
    )
    weight <- dimSums(calcOutput("IO", subtype = "output", aggregate = FALSE)[, 2005, c("feeli", "feelb")], dim = 3)
  } else if (subtype == "tradeConstraints") {
    unit <- "unitless"
    description <- glue::glue(
      "parameter by Nicolas Bauer (2024) for the region specific \\
      trade constraints, values different to 1 activate constraints \\
      and the value is used as effectiveness to varying degrees \\
      such as percentage numbers"
    )
    weight <- NULL
  }

  return(list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    isocountries = isocountries[[subtype]]
  ))
}
