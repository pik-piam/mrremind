#' Convert AGEB data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readJRC_IDEES()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr %>%
#' @importFrom madrat getISOlist
#'
#' @export
convertAGEB <- function(x) {
  add_columns(x, addnm = setdiff(getISOlist(), "DEU"), dim = 1, fill = NA) %>% return()
}
