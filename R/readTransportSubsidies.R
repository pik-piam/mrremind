#' Read transport subsidies data
#' 
#' Read-in transport subsidies csv files as magclass object
#' 
#' @return magpie object of the transport subsidies for BEV, FCEV and PHEV (euros/car) for private and legal entities
#' @author Renato Rodrigues
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="TransportSubsidies")
#' }
#'  

 readTransportSubsidies <- function() {
   
     data <- read.csv("transportSubsidies.csv",sep=";")
     x <- as.magpie(data,datacol=3)
     
   return(x)
 }  
