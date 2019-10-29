#' @title calcTimberShare
#' @description 
#' Calculates the production share of timber from plantations needed to upscale the yield of forest plantations as compared to natural vegetation based on FAO data.
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TimberShare")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcTimberShare <- function() {
  x    <- readSource("TimberShare")
  
  return(list(x=x,weight=NULL,unit="percentage",description="percentage"))
}