#' calcEmiAirPoll
#' calculate Air Pollution Emissions
#' 
#' @return magpie object
#' @author Julian Oeser
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="EmiAirPollLandUse")
#' }
#' 


calcEmiAirPollLandUse <- function(){
 
  x <- readSource("MAgPIE", subtype = "EmiAirPoll")
  
  # add missing rcp-scenarios, use
  # rcp60 for "none" and "rcp85"  
  # rcp45 for "rcp37"
  # rcp26 for "rcp20"
  x_none                 <- x[,,"rcp60"]
  getNames(x_none,dim=3) <- "none"
  x_addRCP                 <- mbind(x[,,"rcp26"],x[,,"rcp45"],x[,,"rcp60"])
  getNames(x_addRCP,dim=3) <- c("rcp20","rcp37","rcp85") 
  # put all together
  y <- mbind(x,x_none,x_addRCP)
  
  return(list(x           = y,
              weight      = NULL,
              unit        = "unit",
              description = "agricultural air pollution"))
  
}
