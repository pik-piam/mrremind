#' @title convertDias
#' @author Aman Malik
convertDias <- function(x,subtype){
  
 getRegions(x) <- toolCountry2isocode(getRegions(x))
  return (x)
  
}