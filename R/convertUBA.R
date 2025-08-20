#' Convert UBA data
#'
#' @param x a magpie object
#' @author Falk Benke
#'
convertUBA <- function(x) {
  return(add_columns(x, addnm = setdiff(getISOlist(), "DEU"), dim = 1, fill = NA))
}
