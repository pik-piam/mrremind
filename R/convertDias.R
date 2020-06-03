#' @title convertDias
#' @author Aman Malik
#' @param x  magpie object to be converted
#' @param subtype  "Employment factors" or "Employment"
#' 
convertDias <- function(x,subtype){
  if (subtype=="Employment factors"){
    #x <- readSource(type = "Dias", subtype = "Employment factors", convert = F) # EFs for coal and coal mining
    
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x,fill=NA)
  }
  if (subtype=="Employment"){
    #x <- readSource(type = "Dias", subtype = "Employment", convert = F) # EFs for coal and coal mining
    getNames(x) <- gsub(getNames(x),pattern = "mine",replacement = "Fuel_supply")
    getNames(x) <- gsub(getNames(x),pattern = "power_plant",replacement = "OM")
    getSets(x)[4] <- "activity"
    x <- add_dimension(x,dim = 3.3,add = "tech",nm = "Coal")
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    x <- toolCountryFill(x,fill=NA)
  }
  return (x)
  
}