#' Convert PYieldCoeff data to ISO country level.
#' 
#' @param x MAgPIE object containing data for fixed regional resolution
#' @return data as MAgPIE object disaggregated to country level
#' @author Isabelle Weindl
#' @examples
#' 
#' \dontrun{ a <- convertPYieldCoeff(x)
#' }
#' 
convertPYieldCoeff <- function(x) {
  y <- toolAggregate(x, "regionmappingPYieldCoeff.csv", weight=NULL)
  return(y)
}
