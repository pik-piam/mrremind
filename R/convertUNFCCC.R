#' Convert UNFCCC data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readUNFCCC()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom madrat toolCountryFill
#'
#' @export
convertUNFCCC <- function(x) {
  x <- toolCountryFill(x, verbosity = 2, no_remove_warning = "EUA")
  return(x)
}
