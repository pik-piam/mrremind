#' Convert subtypes of the IPCC data
#' 
#' Convert subtypes on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing IPCC data on region level
#' @param subtype data subtype. Either "awmsShr", "awmsEfCh4", "awmsParCh4",
#' "nExcrRate"
#' @return IPCC data as MAgPIE object for all subtypes on country level
#' @author Nele Steinmetz
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource("IPCC","awmsShr")
#' a <- readSource("IPCC","awmsEfCh4")
#' a <- readSource("IPCC","awmsParCh4")
#' a <- readSource("IPCC","nExcrRate")
#' }
#' 
convertIPCC <- function(x,subtype) {
   map <- "regionmappingIPCC_livestock.csv"
   map_nExcrRate <- "regionmappingIPCC_nExcrRate.csv"
  
  if(subtype=="awmsShr"){
    x <- x/100 
    y <- toolAggregate(x, map, weight=NULL )
  } else if(subtype=="awmsEfCh4") {
    y <- toolAggregate(x, map, weight=NULL )
  } else if(subtype=="awmsParCh4"){
    y <- toolAggregate(x, map, weight=NULL )
  } else if(subtype=="nExcrRate"){
    y <- toolAggregate(x, map_nExcrRate, weight=NULL )
  } else {stop("no convert script for this subtype")}

  return(y)
}
