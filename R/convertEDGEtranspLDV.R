#' Convert EDGE Transport LDV shares
#' 
#' @param x MAgPIE object containing EDGE Transport LDV shares values at ISO country resolution
#' @return EDGE Transport LDV shares data as MAgPIE object aggregated to country level
#' @author Renato Rodrigues
#' 

convertEDGEtranspLDV <- function(x) {

  x <- x[,,"share_LDV_totliq"]
  
  for (year in getYears(x, as.integer = T)){
    x[,year,] <- as.vector(x[,c(2010),]) + ((0.55 - as.vector(x[,c(2010),]))/(2100-2010))*(year-2010)
  }
  
  #extending values
  x <- time_interpolate(x, integrate_interpolated_years=T, interpolated_year = seq(from = 1990, to = 2100), extrapolation_type = "linear")
  x <- time_interpolate(x, integrate_interpolated_years=T, interpolated_year = c(seq(from = 1970, to = 1989),seq(from = 2101, to = 2150)), extrapolation_type = "constant")
  
  return(x)
}   