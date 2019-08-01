#' toolIso2CellCountries
#' 
#' Select country names of countries which are present on cellular level
#' @param x magpie object on iso country level
#' @return return selected input data
#' @author Kristine Karstens
#' 
#' @importFrom utils read.csv
#' @export


toolIso2CellCountries <- function(x){
  
  CellToCellIso  <- read.csv(toolMappingFile("cell","CountryToCellMapping.csv"))
  IsoCellCountry <- levels(CellToCellIso$iso)
  y              <- x[IsoCellCountry,,]
  
  total          <- dimSums(dimSums(x, dim=c(2,3)), dim=1)
  returned       <- dimSums(dimSums(y, dim=c(2,3)), dim=1)
  lost           <- 1-(returned/total)
  
  if(lost!=0) cat(lost, " of the summed up values of the data set gets lost.\n")
  
  return(y)
}