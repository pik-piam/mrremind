#' Aggregate forest area designated for production values from source
#' FAO_forestry_production
#' 
#' Aggregates forest area designated for production values from source
#' FAO_forestry_production over the magpie-regions. NAs get replaced by zero. A
#' weight for the aggregation is not implemented
#' 
#' 
#' @return writes a magpie object with the new regions to the output-folder
#' defined in the file "exampleconfig.cfg"
#' @author Nele Steinmetz
#' @seealso \code{\link{setConfig}}, \code{\link{readSource}},
#' \code{\link{readFAO_forestry}}, \code{\link{calcOutput}}
calcForestProd <- function() {
  
  x   <- readSource("FAO_forestry","production")
  e   <- readSource("FAO_forestry","extent")
  
  x <- dimSums((x/e)[,c(1990,2000),],dim=2)/2
  x[is.nan(x)] <- 0
  x[x>1] <- 1
  w <- dimSums(e[,c(1990,2000),],dim=2)/2
  
  return(list(x=x,weight=w,unit="-",description="Share of total forest area designated for production on average from 1990-2000 (FRA2010)"))
}
