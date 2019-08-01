#' Convert WBirrigation data
#' 
#' Convert WorldBank-irrigation data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing WBirrigation data country-region
#' resolution
#' @return WBirrigation data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertWBirrigation(x)
#' }
#' 
convertWBirrigation <- function(x) {
  x <- x[!(getRegions(x) %in% c("AFR","ESA")),,]
  y <- toolAggregate(x, "regionmappingIrrigation.csv", weight=NULL)
  return(y)
}
