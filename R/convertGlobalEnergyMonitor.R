#' Convert Global Energy Monitor data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readGlobalEnergyMonitor()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom madrat toolCountry2isocode toolCountryFill
#'
#' @export
convertGlobalEnergyMonitor <- function(x) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  return(toolCountryFill(x, no_remove_warning = "KOS"))
}
