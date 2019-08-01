#' convertEORA
#' 
#' @description Converts readEORA output to complete MAgPIE object
#' @param x MAgpIE object containing EORA data on country level
#' @return EORA data as complete MAgPIE object on country level
#' @author Debbora Leip
#' @seealso \code{\link{readSource}}


convertEORA <- function(x){
  x[is.na(x)] <- 0
  x <- toolCountryFill(x, fill=0)
  return(x)
}