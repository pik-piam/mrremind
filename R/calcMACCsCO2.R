#' Read in abatement potential for CO2 land-use change derived from MAgPIE
#' 
#' Rrange of possible abatement between maximum and minimum emission level in a year
#' 
#' @return MAgPIE object
#' @author David Klein
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("MACCsCO2")
#' 
#' }
calcMACCsCO2 <- function() {
  
  x <- readSource("MAgPIE", subtype = "abatparam_co2",convert=FALSE)
  
  # No weight for aggregation needed since data is global

  return(list(x           = x,
              weight      = NULL,
              unit        = "Percent",
              description = "Abatement potential in percentage for 5$/tC tax steps"))
}
