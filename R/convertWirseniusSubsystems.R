#' Convert Wirsenius Livestock Subsystems
#' 
#' Convert data on livestock subsystems structure. Source: Wirsenius.
#' 
#' @param x the data to be converted? 
#' @param subtype Type of subsystem information that should be read in. 
#' Available types are: "cattlemeat2milk", "meat2egg" and "feed_beefsys_ratio"
#' @return magpie object of selected livestock subsystem data on country level
#' @author Isabelle Weindl
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' a <- readSource("WirseniusSubsystems","cattlemeat2milk")
#' a <- readSource("WirseniusSubsystems","meat2egg")
#' a <- readSource("WirseniusSubsystems","feed_beefsys")
#' }
#' 

convertWirseniusSubsystems <- function(x,subtype) {

  map <- "regionmappingWirseniusSubsystems.csv"  
  
  if(subtype=="cattlemeat2milk"){
    y <- toolAggregate(x, map, weight=NULL )
  } else if(subtype=="meat2egg") {
    y <- toolAggregate(x, map, weight=NULL )
  } else if(subtype=="feed_beefsys"){
    y <- toolAggregate(x, map, weight=NULL )
  } else {stop("no convert script for this subtype")}
  
  return(y)
} 
