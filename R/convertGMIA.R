#' @title convertGMIA
#' @description Convert Global Map on Irrigated Area Data
#' 
#' Convert subtypes on ISO country level.
#' 
#
#' @param x MAgPIE object containing IrrigatedArea data on Country level
#' @param subtype : No subtype needed
#'
#' @return Global Map on Irrigation data as MAgPIE object on country level Missing values are added as NA
#' 
#' @author Stephen Wirth
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#'  a <- readSource("GlobalIrrigationMap", "all_data_national")
#' a <- readSource ("GMIA", "aei_pct", convert=F)
#' a <- readSource ("GMIA", "aei_pct", convert="correctonly")
#' }
#'
#' 
#' 
convertGMIA <- function(x,subtype) {
  
  
  if(subtype=="all_data_national")
    {
    map <- toolMappingFile(type="regional", readcsv=T, name="regionmappingMAgPIE.csv")
 # map <- "regionmappingMAgPIE.csv"
    map$X[grep("Virgin Islands, U.S.", map$X)] <- "Virgin Islands, U"
    map$X <- toupper(map$X)
    getRegions(x) <- toupper(getRegions(x))
  y <- toolAggregate(x, rel = map, from=1 ,to=2, partrel = T)
  y <- toolCountryFill(y)
  
}
 else{
   map <- toolMappingFile(type="cell", readcsv=T, name="CountryToCellMapping.csv")
 y <- toolAggregate(x, rel=map, from=1, to=3, partrel = T)
  # stop(paste0("No conversion for subtype ", subtype, " available. Try convert=correctonly, for aggregation to 0.5 degree resolution, or convert=F for 5 armin resolution"))
y <- toolCountryFill(y)
}
  return(y)
}