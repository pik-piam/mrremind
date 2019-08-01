#' Convert Bodirsky data
#' 
#' Convert Bodirsky data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing Bodirsky data country-region resolution
#' @return Bodirsky data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertBodirsky(x)
#' }
#' 
convertBodirsky <- function(x) {
  y <- toolAggregate(x, "regionmappingBodirsky.csv", weight=NULL )
  return(y)
}
