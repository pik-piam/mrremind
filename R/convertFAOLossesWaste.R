#' Convert data on  food losses and waste from FAO for several commodity groups
#' 
#' Convert data on  food losses and waste on ISO country level.
#' 
#' 
#' @param x MAgPIE object containing data on  food losses and waste at mixed country-region
#' resolution
#' @param subtype Steps of the food supply chain where food losses and waste occur. Available types are:
#' \itemize{ 
#' \item \code{Consumption}: consumption level
#' }
#' @return Data on  food losses and waste as MAgPIE object at ISO country level
#' @author Isabelle Weindl
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource(type="FAOLossesWaste",subtype="Consumption")
#' }

convertFAOLossesWaste <- function(x,subtype) {
  
  if(subtype == "Consumption"){
    
    y <- toolAggregate(x, "regionmappingFAOLossesWaste.csv", weight=NULL )
    
  }else {
    stop("Not a valid subtype!")
  } 
  
  return(y)
}