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

  isocountries <- c(
    "subConvergenceRollback" = TRUE,
    "tradeConstraints" = FALSE,
    "taxConvergence" = TRUE,
    "taxConvergenceRollback" = TRUE
  )

  x <- readSource("ExpertGuess", subtype = subtype, convert = isocountries[[subtype]])

  if (subtype == "tradeConstraints") {

    unit <- "unitless"
    description <- glue::glue(
      "parameter by Nicolas Bauer (2024) for the region specific \\
      trade constraints, values different to 1 activate constraints \\
      and the value is used as effectiveness to varying degrees \\
      such as percentage numbers"
    )
    weight <- NULL

  } else if (subtype == "taxConvergence") {


    # convert data from $2005 to $2017
    x <- GDPuc::toolConvertGDP(
      gdp = x,
      unit_in = "constant 2005 US$MER",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )

    unit <- "US$2017/GJ"
    description <- glue::glue("Tax convergence level for specific regions, year \\
                              and final energy type")
    weight <- x
    weight[, , ] <- 1

  } else if (subtype == "taxConvergenceRollback") {

    unit <- "US$2017/GJ"
    description <- glue::glue("Tax convergence level for specific regions, year \\
                              and final energy type in rollback scenario")
    weight <- x
    weight[, , ] <- 1

  } else if (subtype == "subConvergenceRollback") {

    unit <- "US$2017/GJ"
    description <- glue::glue("Subsidy convergence level for specific regions, \\
                              year, emission sectors and final energy type in \\
                              rollback scenario")
    weight <- x
    weight[, , ] <- 1

  }

  return(list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    isocountries = isocountries[[subtype]]
  ))
}
