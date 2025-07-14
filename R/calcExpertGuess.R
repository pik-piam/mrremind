#' Calculate expert guesses
#'
#' @author Falk Benke
#'
#' @param subtype must be one of
#' 'subConvergenceRollback'
#' 'tradeConstraints'
#' 'taxConvergence'
#' 'taxConvergenceRollback'
#' @export
calcExpertGuess <- function(subtype) {
  subtypes <- c(
    "subConvergenceRollback",
    "tradeConstraints",
    "taxConvergence",
    "taxConvergenceRollback"
  )

  if (!(subtype %in% subtypes)) {
    stop("Invalid subtype. Supported subtypes: ", paste0(subtypes, collapse = ", "))
  }

  x <- readSource("ExpertGuess", subtype = subtype)

  if (subtype == "tradeConstraints") {

    unit <- "unitless"
    description <- glue::glue(
      "parameter by Nicolas Bauer (2024) for the region specific \\
      trade constraints, values different to 1 activate constraints \\
      and the value is used as effectiveness to varying degrees \\
      such as percentage numbers"
    )
    weight <- NULL
    isocountries <- FALSE

  } else if (subtype == "taxConvergence") {

    unit <- "US$2017/GJ"
    # TODO: improve description
    description <- "Tax convergence level for specific regions, year and final energy type"
    weight <- x
    weight[, , ] <- 1
    isocountries <- TRUE

  } else if (subtype == "taxConvergenceRollback") {

    unit <- "US$2017/GJ"
    # TODO: improve description
    description <- "Tax convergence level for specific regions, year and final energy type"
    weight <- x
    weight[, , ] <- 1
    isocountries <- TRUE

  } else if (subtype == "subConvergenceRollback") {

    unit <- "US$2017/GJ"
    # TODO: improve description
    description <- "Tax convergence level for specific regions, year and final energy type"
    weight <- x
    weight[, , ] <- 1
    isocountries <- TRUE

  }

  return(list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    isocountries = isocountries
  ))
}
