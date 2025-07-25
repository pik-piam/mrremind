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
#'
#' @export

convertEmber <- function(x) {

  # add missing countries
  x <- toolCountryFill(x, fill = 0, verbosity = 2, no_remove_warning = "XKX")

  # replace NA by 0 to enable aggregation over incomplete regions as only
  # small countries are missing
  x[is.na(x)] <- 0

  return(x)
}
