#' Convert European Environment Agency (EEA) data
#' 
#' Read-in European Environment Agency (EEA) data on ETS emissions as magclass object
#' 
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. "ETS"
#' @return magpie object of European Environment Agency (EEA) ETS emissions (GtCO2) 
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="EEA_EuropeanEnvironmentAgency",subtype="ETS")
#' }
#'  
#' @importFrom magclass as.magpie
#' 
 
convertEEA_EuropeanEnvironmentAgency <- function(x,subtype) {
  if (subtype == "ETS") { 
    # fill up zero countries
    x <- toolCountryFill(x)
    #remove NAs
    x[is.na(x)] <- 0
  }
  return(x)
}
