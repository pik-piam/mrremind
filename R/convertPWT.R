#' Convert PWT data
#' 
#' Convert PWT data on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing PWT data country-region resolution
#' @return PWT data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' 
#' \dontrun{ a <- convertPWT(x)
#' }
#' 
convertPWT <- function(x) {
  # fill all missing countries with 0
  x <- toolCountryFill(x,fill=0)
  return(x)
}  
