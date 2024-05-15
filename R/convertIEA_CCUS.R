#' Convert IEA CCUS data
#'
#' @md
#' @param x A magclass object returned from readIEA_CCUS().
#'
#' @return A magclass object.
#'
#' @author Anne Merfort, Falk Benke
#'
#'
#' @export
convertIEA_CCUS <- function(x) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  return(x)
}
