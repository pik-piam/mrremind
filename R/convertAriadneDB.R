#' Convert Ariadne DB data
#' @description  convert Ariadne database data
#' @return A [`magpie`][magclass::magclass] object.
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readAriadneDB()`].
#' @author Felix Schreyer

convertAriadneDB <- function(x) {
  getItems(x, dim = 1) <- "DEU"
  x <- add_columns(x, addnm = setdiff(getISOlist(), "DEU"), dim = 1, fill = NA)
  return(x)
}
