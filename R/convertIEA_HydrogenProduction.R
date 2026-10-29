#' Convert IEA hydrogen capacities
#'
#' @md
#' @param x A magclass object returned from readIEA_HydrogenCapacities().
#'
#' @return A magclass object.
#'
#' @author Simon Krogmann
#'
convertIEA_HydrogenProduction <- function(x) {
  return(toolCountryFill(x, fill = 0, verbosity = 2))
}
