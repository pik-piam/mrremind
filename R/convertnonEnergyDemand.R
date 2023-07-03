#' Converts Final energy demand for feedstocks (non-energy use)
#' 
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing country disaggregated data
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ a <- convertnonEnergyDemand(x)
#' }
#' 

convertnonEnergyDemand <- function(x) {
  
  # Replacing NA values with zero
  x[is.na(x)] <- 0
  
  # weight to convert to country data values
  IO <- calcOutput("IO",subtype="output",aggregate=FALSE)[,2010,c("seliqfos.fehoi","segafos.fegai","sesofos.fesoi")]
  w <- setNames(IO[,,c("seliqfos.fehoi.tdfoshoi","segafos.fegai.tdfosgai","sesofos.fesoi.tdfossoi")],c("chemicals.fehos","chemicals.fegas","chemicals.fesos"))
  
  # converting from REMIND-EU regions to country data using industry FE as weight
  y <- toolAggregate(x, toolGetMapping(type = "regional", name = "regionmapping_21_EU11.csv", where = "mappingfolder")[,c(1:3)], weight=w) 

return(y)
}
