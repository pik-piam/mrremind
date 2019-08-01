#' Read BGR oil, gas, coal and uranium reserves data
#' 
#' Read-in BGR csv files as magclass object
#' 
#' 
#' @param subtype data subtype. Either "oil", "gas", "coal" or "uranium".
#' @return magpie object of the BGR (Federal Institute for Geosciences and Natural Resources) data of reserves of oil, gas, coal and uranium per country.
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="BGR",subtype="oil")
#' }
#'  

 readBGR <- function(subtype) {
   if (subtype == "oil") {
     #Reading oil resources per country values in 2009 [Mt] from csv
     data <- read.csv("oil_reserves.csv",sep=";")
   } else if (subtype== "gas") {
     #Reading gas resources per country values in 2009 [Mrd. m³] from csv
     data <- read.csv("gas_reserves.csv",sep=";")
   } else if (subtype== "coal") {
     #Reading coal resources per country values in 2009 [Mt] from csv
     data <- read.csv("coal_reserves.csv",sep=";")
   } else if (subtype== "uranium") {
     #Reading uranium resources per country values in 2009 [kt U] from csv
     data <- read.csv("uranium_reserves.csv",sep=";")
   } else {
     stop("Not a valid subtype!")
   }
   x <- as.magpie(data,temporal=0,spatial=1,datacol=2)
   return(x)
 }  
