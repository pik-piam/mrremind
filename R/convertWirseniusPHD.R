#' Convert WirseniusPHD data
#' 
#' Convert Wirsenius PHD data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing Wirsenius PHD data country-region
#' resolution
#' @return WirseniusPHD data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertWirseniusPHD(x)
#' }
#' 
convertWirseniusPHD <- function(x) {
  y <- toolAggregate(x, "regionmappingWirseniusPHD.csv", weight=NULL )
  return(y)
}
