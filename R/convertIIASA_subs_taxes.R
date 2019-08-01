#' Convert IIASA_subs_taxes data
#' 
#' Convert IIASA subsidy and taxes data on ISO country level (removes countries
#' not part of 249 oficial ISO countries and fills missing with zeros).
#' 
#' 
#' @param x MAgPIE object containing IIASA subsidies and taxes data in country
#' resolution
#' @return IIASA_subs_taxes data as MAgPIE object aggregated to country level
#' @author Christoph Bertram
#' @examples
#' 
#' \dontrun{ a <- convertIIASA_subs_taxes(x)
#' }
#' 
convertIIASA_subs_taxes <- function(x) {
  # fill all missing countries with 0, remove all countries not part of oficial 249 ISO countries
  x <- toolCountryFill(x,fill=0)
  return(x)
}  
