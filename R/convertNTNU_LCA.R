#' Convert NTNU_LCA data
#' 
#' Convert NTNU_LCA data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing NTNU_LCA data country-region resolution
#' @return NTNU_LCA data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertNTNU_LCA(x)
#' }
#' 
convertNTNU_LCA <- function(x) { 
  y <- toolAggregate(x, "regionmappingNTNU_LCA.csv", weight=NULL)
  return(y)
}
