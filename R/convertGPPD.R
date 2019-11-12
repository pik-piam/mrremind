#' Capacities from the Global Power Plant Database
#' @param x MAgPIE object to be converted
#' @return Magpie object, country capacities (MW) from the Global Power Plant Database for different fuels.
#' @author Aman Malik
#' @import dplyr

convertGPPD <- function(x){
  # x <- readSource("GPPD",convert = F)
  x <- toolCountryFill(x,fill = 0)# data available for 164 countries. For countries with no data, put value as zero
  getYears(x) <- 2017
}







