#' Read IMPACT3.2.2World_Price
#' 
#' Read-in world prices data csv file as magclass object
#' 
#' 
#' @return magpie object of the world prices from the IMPACT model for different SSP scenarios
#' @author Mishko Stevanovic
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="IMPACT3.2.2World_Price")
#' }
#' 
#' @importFrom readxl read_excel 
#' @importFrom magclass as.magpie

readIMPACT3.2.2World_Price <- function(){
  data <- as.data.frame(read_excel("IMPACT 3.2.2 World Price-2.xlsx", skip=6))
  data <- data[!is.na(data[[1]]),]
  data$scenario <-sub(".","p",data$scenario,fixed=TRUE)
  data <- as.magpie(data)
  return(data)
}
 