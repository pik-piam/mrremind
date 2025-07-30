#' Convert Ember data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readEmber()`].
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

  # replace NA by 0 to enable aggregation over incomplete regions
  x[is.na(x)] <- 0

  return(x)
}
