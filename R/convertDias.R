#' @title convertDias
#' @author Aman Malik
#' @param x  magpie object to be converted
#' @param subtype  "Employment factors" or "Employment"
#' 
convertDias <- function(x,subtype){
if (subtype=="Employment factors"|subtype=="Employment")
 #x <- readSource(type = "Dias", subtype = "Employment factors", convert = F) # EFs for coal and coal mining
 getRegions(x) <- toolCountry2isocode(getRegions(x))
 x <- toolCountryFill(x,fill=0)

 return (x)
  
}