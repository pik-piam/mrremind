
#' Read Final energy demand for feedstocks (non-energy use)
#' 
#' @return magpie object of region dependent data
#' @author Renato Rodrigues
#' 
#' @seealso \code{\link{readSource}}
#' 
#' @examples
#' 
#' \dontrun{ a <- readSource(type="nonEnergyDemand")
#' }
#' 
readnonEnergyDemand <- function() {
  
  x <- read.csv("Final_Energy_demand_for_non_energy_use_in_industry.csv")
  x <- as.magpie(x,spatial=2,temporal=1)
  
  return(x)
}
