#' Convert Ember data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readHRE()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
#' @importFrom madrat toolCountryFill
#'
#' @export

convertEmber <- function(x) {

  # add missing countries
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  # replace NA by 0
  x[is.na(x)] <- 0

  return(x)
}
