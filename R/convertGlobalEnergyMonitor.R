#' Convert Global Energy Monitor data
#'
#' @author Rahel Mandaroux, Falk Benke
#'
#' @param x A magclass object returned from readGlobalEnergyMonitor().
#' @importFrom madrat toolCountry2isocode toolCountryFill
#'
#' @export
convertGlobalEnergyMonitor <- function(x) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x <- toolCountryFill(x, no_remove_warning = "KOS", verbosity = 2)
  return(x)
}
