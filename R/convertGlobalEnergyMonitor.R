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
  mapping <- c(
    "Åland Islands" = "ALA",
    "DR Congo" = "COD",
    "Republic of the Congo" = "COG",
    "The Gambia" = "GMB",
    "Virgin Islands (U_S_)" = "VIR"
  )
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), mapping = mapping)
  return(toolCountryFill(x, no_remove_warning = "KOS"))
}
