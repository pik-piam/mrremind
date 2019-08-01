#' convertHerridge
#' 
#' Converts the Data from Herridge et al. to fit the common country list.
#' Source: Herridge D. F., Peoples M. B., Boddey R. M.: Global inputs of
#' biological nitrogen fixation in agricultural systems
#' 
#' 
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing the share of Nr derived from fixation for
#' each country and each commodity.
#' @author Stephen Wirth
#' @examples
#' 
#' 
#'   \dontrun{
#'     x <- readSource("Herridge")
#'   }
#' 
#' 
convertHerridge <- function(x) {
  
  x  <- toolCountryFill(x,fill=0)
  
  return(x)
}
