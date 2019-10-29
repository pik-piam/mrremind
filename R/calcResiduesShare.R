#' calcResiduesShare
#' calculate residues share
#' 
#' @return magpie object
#' @author Julian Oeser, David Klein
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="ResiduesShare")
#' }
#' 


calcResiduesShare <- function() {
 
  x <- readSource("REMIND_11Regi", subtype = "residuesShare")

  x <- x[,,"pebiolc"] # ommit pebios and pebioil for magpie_40 because they come separately

  
  pop <- calcOutput("Population",years=2005,aggregate=FALSE)[,,"pop_SSP2"]
  
  return(list(x      = x,
              weight = pop,
              unit        = "share",
              description = "regional share in global bioenergy potential"))
  
}
