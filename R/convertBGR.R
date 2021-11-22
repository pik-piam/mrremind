#' Converts BGR oil, gas, coal and uranium reserves data
#' 
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "oil", "gas", "coal" or "uranium".
#' @return A MAgPIE object containing BGR (Federal Institute for Geosciences and Natural Resources) country reserves disaggregated data of oil, gas, coal and uranium.
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ a <- convertBGR(x,subtype="oil")
#' }
#'  

 convertBGR <- function(x,subtype) {
   # rename countries to REMIND iso codes
   getRegions(x) <- toolCountry2isocode(getRegions(x))
   x[is.na(x)] <- 0 # setting NA values to zero
   x  <- toolCountryFill(x,fill=0,verbosity=2) # fill countries with no data
   return(x)
 }
 
 
