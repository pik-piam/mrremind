#' Convert FAO data for forest
#' 
#' Convert the FAO data to fit to the common country list. Missing countries
#' are added and assigned the value zero.
#' 
#' 
#' @param x MAgPIE object containing values of the forest data
#' @return Forest within protected areas data as MAgPIE object with common
#' country list
#' @author Nele Steinmetz

convertFAO_forestry <- function(x){
  x[is.na(x) | is.nan(x)] <- 0
  x=x/1000
  data <- toolCountryFill(x, fill=0)
  return(data)
}
