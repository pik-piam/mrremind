#' convert HDD_CDD
#' 
#' Convert historic and scenario based HDD CDD data to data on ISO country
#' level.
#' 
#' 
#' @param x MAgPIE object containing HDD CDD values at ISO country resolution
#' @return HDD CDD data as MAgPIE object aggregated to country level
#' @author Antoine Levesque

convertHDD_CDD <- function(x) {
  xadd <- toolCountryFill(x,0)
  
  return(xadd)
} 
