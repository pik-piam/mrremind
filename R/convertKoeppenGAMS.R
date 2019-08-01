#' @title convertKoeppenGAMS
#' @description koeppen geiger climate data compatible to GAMS usage.
#' @param x MAgPIE object containing original values
#' @param subtype Not available.
#' @return Koeppen geiger climate zone classsification data
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("readKoeppenGAMS",convert="FALSE")
#' }
#' 
convertKoeppenGAMS<-function(x,subtype){
  stop("Convert functions for Koeppen data for GAMS are not written yet because an aggregate climate classification doen't make sense. The data was read diretly on a cellular level. Set convert=FALSE")
  return(x)
}  