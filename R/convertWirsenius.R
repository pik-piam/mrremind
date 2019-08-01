#' Convert Wirsenius data
#' 
#' Convert Wirsenius data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing Wirsenius data country-region resolution
#' @return Wirsenius data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertWirsenius(x)
#' }
#' 
convertWirsenius <- function(x) {
  y <- toolAggregate(x, "regionmappingWirsenius.csv", weight=NULL)
  return(y)
}
