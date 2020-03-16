#' Convert IEA PVPS data from REMIND regions to iso countries
#' @description maps to iso countries
#' @param x MAgPIE object to be converted
#' @param subtype type of data read from IEA PVPS
#' @return Magpie object with IEA PVPS investment cost per country
#' @author Felix Schreyer



convertIEA_PVPS <- function(x, subtype) {
  
  regmapping <- toolGetMapping("regionmappingH12.csv",where = "mappingfolder",type = "regional")
  x_iso <- toolAggregate(x, regmapping)
 return(x_iso)
}  
