#' Calculate CCS bound indicator for 2025 and 2030
#' 
#' 
#' 
#' @author Jessica Strefler, Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}

calcCCSbounds <- function() {
   
  data <- readSource("ExpertGuess", subtype="CCSbounds")
  getNames(data) <- NULL
  
  return(list(x=data,weight=NULL,
              unit="TW",
              description="CCS bound indicator"))
}
