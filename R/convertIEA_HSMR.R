#' convert IEA Hydro Special Market Report
#'
#' @param x a magclass object returned from `readIEA_HSMR()`
#' @author Pascal Weigmann

convertIEA_HSMR <- function(x) {

  x <- x["World", , invert = TRUE]
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
