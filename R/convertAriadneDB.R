#' Convert AGEB data
#' @description  convert Ariadne database data
#' @author Felix Schreyer

convertAriadneDB <- function(x) {
  getItems(x, dim=1) <- "DEU"
  add_columns(x, addnm = setdiff(getISOlist(), "DEU"), dim = 1, fill = NA) %>% return()
}
