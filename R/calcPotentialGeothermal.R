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
  GWh_2_EJ <- 3.6e-6
  # read IRENA country geothermal potential
  gen <- readSource(type = "IRENA", subtype = "Generation")[, 2020, "Geothermal"] * GWh_2_EJ
  
  # Simple assumption: Goethermal country potential for the century is 15 times the 2015 observed generation
  # RP: This factor allows 2030/2050 NPI capacity targets to be met, but still no region can produce more than 1.5EJ/yr
  # Given that geohdr is still a nascent technology, allowing stronger upscaling seems justified
  maxprod <- gen * 15
  
  maxprod <- setNames(maxprod, "maxprod")
  getYears(maxprod) <- NULL
  
  return(list(x                 = maxprod,
              weight            = NULL,
              unit              = "EJ/a",
              description       = "Geothermal Potential"
  ))
}
