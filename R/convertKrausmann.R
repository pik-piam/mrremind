#' Convert Krausmann data
#' 
#' Convert Krausmann data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing Krausmann data region resolution
#' @return Krausmann data as MAgPIE object aggregated/disaggregated to country
#' level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertKrausmann(x)
#' }
#' 
convertKrausmann <- function(x) {
  y <- toolAggregate(x, "regionmappingKrausmann.csv", weight=NULL)  
  return(y)
}
