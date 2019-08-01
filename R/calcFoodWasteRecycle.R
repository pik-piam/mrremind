#' Calculate FoodWasteRecycle
#' 
#' Reads in information on the share of food waste recycled for feed use of the
#' source WirseniusPHD. Implementation of a weight calculated by
#' calcDemandAgriculture (y2000, food.ssp2).
#' 
#' 
#' @return MAgPIE object
#' @author Nele Steinmetz, Isabelle Weindl, Benjamin Leon Bodirsky
#' @seealso \code{\link{calcOutput}}, \code{\link{readWirseniusPHD}},
#' \code{\link{convertWirseniusPHD}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FoodWasteRecycle")
#' 
#' }
#' @importFrom magclass getNames

calcFoodWasteRecycle <- function() {
  x    <- readSource("WirseniusPHD")[,,"3_20.Non-eaten food.Non-eaten food",pmatch=TRUE]
  getNames(x) <- NULL
  weight<-collapseNames(setNames(calcOutput("HHFoodWaste",aggregate = FALSE)[,"y2000","ge"],NULL))
  return(list(x=x,
              weight=weight,
              unit="-",
              description="share of food waste used as feed"
              ))
}
