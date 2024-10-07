#' convert IAEA Power Reactor Information System
#'
#' @param x a magclass object returned from `readIAEA_PRIS()`
#' @author Pascal Weigmann

convertIAEA_PRIS <- function(x) {

  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
