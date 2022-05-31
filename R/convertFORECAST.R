#' Convert FORECAST data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readFORECAST()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr %>%
#' @importFrom madrat getISOlist
#'
#' @export
convertFORECAST <- function(x) {
  add_columns(x, addnm = setdiff(getISOlist(), "DEU"), dim = 1, fill = NA) %>% return()
}
