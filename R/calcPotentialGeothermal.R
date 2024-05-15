#' Calculate Geothermal potential
#' 
#' Provides geothermal potential data
#' 
#' 
#' @return geothermal potential data MAgPIE object
#' @author Renato Rodrigues
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("PotentialGeothermal")
#' 
#' }
#' 

calcPotentialGeothermal <- function() {
  
  # read IRENA country geothermal potential
  gen <- readSource(type="IRENA",subtype="Generation")[,2015,"Geothermal"] # in GWh
  
  gen <- gen / 1000 * 0.0036 # 1000 Gigawatt Hours (1 Terawatt Hour) to Exajoules = 0.0036
  
  # Simple assumption: Goethermal country potential for the century is 5 times the 2015 observed generation
  maxprod <- gen*5
  
  maxprod <- setNames(maxprod, "maxprod")
  getYears(maxprod) <- NULL
  
  return(list(x                 = maxprod,
              weight            = NULL,
              unit              = "EJ/a",
              description       = "Geothermal Potential"
  ))
}
