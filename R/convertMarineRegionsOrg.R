#' Convert Exclusive Economic Zone (EEZ) size data
#'
#' @param x MAgPIE object to be converted
#'
#' @author Tabea Dorndorf
#'
convertMarineRegionsOrg <- function(x) {
  x <- toolCountryFill(x, fill = NA, verbosity = 2)
  return(x)
}
