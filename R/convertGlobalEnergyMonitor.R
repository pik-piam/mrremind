#' Convert Global Energy Monitor data
#'
#' @author Rahel Mandaroux, Falk Benke
#'
#' @param x A magclass object returned from readGlobalEnergyMonitor().
#'
#' @export
convertGlobalEnergyMonitor <- function(x) {
  x <- x["Bonaire, Sint Eustatius, and Saba", , , invert = T]
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x <- toolCountryFill(x,
    no_remove_warning = "KOS",
    verbosity = 2, fill = 0
  )
  return(x)
}
