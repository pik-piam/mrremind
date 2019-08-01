#' Read DemandModel
#' 
#' Read-in an csv that contains data from a DemandModel as magclass object
#' 
#' 
#' @return magpie object of the DemandModel data
#' @author Lavinia Baumstark
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="DemandModel")
#' }
#' @importFrom magclass read.magpie
readDemandModel <- function() {
  return(read.magpie("FoodDemand.mz"))
}  
