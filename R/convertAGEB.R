#' Convert AGEB data
#'
#' @param x a magpie object
#'
#' @author Falk Benke
#'
convertAGEB <- function(x) {
  x <- add_columns(x, addnm = setdiff(getISOlist(), "DEU"), dim = 1, fill = NA)
  return(x)
}
