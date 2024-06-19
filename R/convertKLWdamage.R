#' convert KLW damage
#' fills in countries for which no damage parameters are provided, setting parameters to zero
#' @author Franziska Piontek
#' @param x is MAgPIE object containing the damage parameters from KLW
#' @return MAgPIE object containing values for all 249 ISO countries

convertKLWdamage <- function(x) {
  out <- toolCountryFill(x, fill = 0, verbosity = 2, no_remove_warning = c("GLB"))
  return(out)
}
