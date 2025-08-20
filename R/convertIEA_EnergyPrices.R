#' Convert IEA Energy Prices
#'
#' @author Falk Benke
#' @importFrom madrat toolCountry2isocode
convertIEA_EnergyPrices <- function(x) {
  # remove regions
  x <- x[c("OECDEUR", "OECDTOT"), , invert = TRUE]
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))

  # remove quarterly data
  x <- x[, !grepl("^[1-4]Q", getYears(x)), invert = TRUE]

  x <- toolCountryFill(x, NA, verbosity = 2)
  return(x)
}
