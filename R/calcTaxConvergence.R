#' @title calc Tax Convergence
#' @description tax convergence levels for specific regions
#'
#' @return magpie object of the tax convergence levels
#' @author Renato Rodrigues
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TaxConvergence")
#' }
#' 

calcTaxConvergence <- function(){
  
  # Read tax convergence levels at specific year and final energy type
  taxConvergence <- readSource("REMIND_11Regi", subtype="taxConvergence")
  # average weight
  w <- new.magpie(getRegions(taxConvergence),getYears(taxConvergence),getNames(taxConvergence),fill=1)
  # Return tax convergence levels aggregated to selected REMIND regions
  return(list(x=taxConvergence, weight=w,
              unit="$/GJ", 
              description="Tax convergence level for specific regions, year and final energy type"              
  ))
  
}
