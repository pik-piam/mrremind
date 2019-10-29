#' @title calcPYieldSlope
#' @description provides slope for calculating pasture intensification
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Isabelle Weindl
#' @seealso
#' \code{\link{readPYieldCoeff}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("PYieldSlope")
#' }

calcPYieldSlope<-function(){
  x<-collapseNames(readSource("PYieldCoeff")[,,"slope"])
  past.land<-calcOutput("LanduseInitialisation", aggregate=FALSE)[,"y1995","past"]
  return(list(x=x,weight=past.land,unit="",description="slope of pasture intensification"))
}