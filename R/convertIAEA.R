#' Nuclear data from world-nuclear.org
#' @description  Data on currently operating and under-construction nuclear power plants, reactors planned and proposed, 
#' electricity generation from nuclear  
#' @author Christoph Bertram
 #' @param x MAgPIE object to be converted
 
convertIAEA <- function(x) {
      
      # remove world data
      x <- x["WORLD",,,invert=TRUE]
  
      # rename countries into ISO
      getRegions(x) <- toolCountry2isocode(getRegions(x))
      # fill missing countries
      x <- toolCountryFill(x,fill=0,verbisity=2)
      
      return(x)
}  
