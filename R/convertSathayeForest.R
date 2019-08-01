#' Convert Sathaye Forest data
#' 
#' Convert Sathaye Forest data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing Sathaye Forest data region resolution
#' @return Sathaye Forest data as MAgPIE object aggregated/disaggregated to
#' country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertSathayeForest(x)
#' }
#' 
convertSathayeForest <- function(x) {
 
  # convert data from $US-2000 into $US-2004
  x <- x * 1.091
  y <- toolAggregate(x, "regionmappingSathayeForest.csv", weight=NULL )
   
  return(y)
}
