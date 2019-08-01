#' Convert data from the EAT Lancet Comission
#' 
#' Convert data from the EAT Lancet Comission to ISO country level.
#' 
#' 
#' @param x MAgPIE object containing EAT Lancet data at mixed country-region
#' resolution
#' @param subtype Type of EAT Lancet data that should be read. Available types are:
#' \itemize{ 
#' \item \code{cons_data}: Consumption analysis ("EAT_Lancet_cons_data.csv")
#' }
#' @return EAT Lancet data as MAgPIE object at ISO country level
#' @author Isabelle Weindl
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{
#' a <- readSource(type="EATLancet",subtype="cons_data")
#' }

convertEATLancet <- function(x,subtype) {
  
if(subtype == "cons_data"){
  
    y <- toolAggregate(x, "regionmappingEATLancet.csv", weight=NULL )
    
  }else {
    stop("Not a valid subtype!")
  } 
  
  return(y)
}
    
    

    
    

    
    
    
