#' Convert Bijl et. al 2017 data
#' Update dd-Jmm-jjjj - Please add comment if changes made here (Abhi)
#' 
#' @param x MAgPIE object containing original values
#' @param subtype Bijl data subtype.
#' @return Data as MAgPIE object with common country list
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}},
#' @examples
#' 
#' \dontrun{ a <- readSource("Bijl2017","demand",convert=FALSE)}
#' @importFrom magclass magpiesort
#' 
#' @importFrom magclass getRegionList<-

convertBijl2017<-function(x,subtype){
  stop("Convert functions for Bijl are not written yet due to missing mapping. Set convert=FALSE. Orignal MAgPIE object is returned.")
  return(x)
}  