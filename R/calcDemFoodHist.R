#' Calculate food demand based on the output of the Demand Model
#' 
#' This function prepares historical food demand values for use. As the source
#' data already provides all required information this function purely selects
#' the needed data.
#' 
#' 
#' @return Historical food demand
#' @author Isabelle Weindl
#' @seealso \code{\link{calcOutput}}, \code{\link{readDemandModel}},
#' \code{\link{convertDemandModel}}
#' @examples
#' 
#' \dontrun{ 
#' a <- calcOutput("DemFoodHist")
#' 
#' }
#' 
calcDemFoodHist <- function() {
  x <- readSource("DemandModel")[,,"food.history"]
  x[is.nan(x)] <- 0
  x[is.na(x)] <- 0
  x <-collapseNames(x)
 
  return(list(x=x, 
              weight=NULL, 
              unit="PJ", 
              description="historical food demand"))
}
