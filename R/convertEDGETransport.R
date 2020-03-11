#' Convert EDGEtransport
#' 
#' Ship EDGETransport data through, as already on ISO level
#' 
#' @param subtype EDGE entries 
#' @param x MAgPIE object containing EDGE values at ISO country resolution
#' @return EDGETransport data as MAgPIE object aggregated to ISO level
#' @author Marianna Rottoli
#' 

convertEDGETransport = function(x, subtype) {
  
  if (subtype %in% c("esCapCost", "fe_demand_tech", "fe2es", "UCD_NEC_iso", "harmonized_intensities", "value_time", "SW", "pref")) {
    ## magpie object creates NA whenever the initial dt is not symmetric (entry absent in ISO1 but exists in ISO2)
    ## the NAs are therefore converted to 0
    x[is.na(x)] <- 0
  }
  
  result = x
  return(result)
}  
