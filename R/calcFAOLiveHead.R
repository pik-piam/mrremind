#' Calculate FAO Living Animals
#' 
#' Provides the FAO Living Animals data. No changes to the content have been
#' done, besides changes to the countries list.
#' 
#' 
#' @return FAO Living Animals data and corresonding weights as a list of two
#' MAgPIE objects
#' @author Ulrich Kreidenweis
#' @seealso \code{\link{calcOutput}}, \code{\link{readFAO}},
#' \code{\link{convertFAO}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LiveHead")
#' 
#' }
#' 
calcFAOLiveHead <- function() {
  x <- readSource("FAO","LiveHead")
  return(list(x=x, weight=NULL, unit="in animal numbers if not stated otherwise in dimension name", description="FAO Live Animals data"))
}

