#' Convert TCdamage
#' fills in countries not affected by tropical cyclones (TC), setting parameters to zero
#' @author Franziska Piontek
#' @param x is MAgPIE object containing the damage parameters for the TC-prone countries
#' @return MAgPIE object containing values for all 249 ISO countries

convertTCdamageKrichene <- function(x){
	out <- toolCountryFill(x, fill = 0, verbosity = 2, no_remove_warning = c("GLB"))
	return(out)
}
