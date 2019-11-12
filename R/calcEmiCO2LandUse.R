#' EmiCO2LandUse
#' calculate co2 emissions from land use change
#' 
#' @return magpie object
#' @author Julian Oeser
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="EmiCO2LandUse")
#' }
#' 


calcEmiCO2LandUse <- function(){
 
  x <- readSource("MAgPIE", subtype = "co2tax")
  x <- collapseNames(x)
  
  return(list(x           = x,
              weight      = NULL,
              unit        = "GtC",
              description = "limit for abatement of CO2 landuse change emissions in REMIND, values are derived from MAgPIE runs with very strong mitigation"))
  
}
