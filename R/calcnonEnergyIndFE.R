#' Final energy demand for feedstocks (non-energy use)
#' 
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Renato Rodrigues
#' 
#' @seealso [`calcOutput()`][madrat::calcOutput].
#' @md
#' 
calcnonEnergyIndFE <- function() {
  
  x <- readSource("nonEnergyDemand")
  
  return(list(x           = x,
              weight      = NULL,
              unit        = "EJ",
              description = "Final energy demand for feedstocks (non-energy use)"))
}
