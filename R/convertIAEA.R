
convertIAEA <- function(x) {
      
      # remove world data
      x <- x["WORLD",,,invert=TRUE]
  
      # rename countries into ISO
      getRegions(x) <- toolCountry2isocode(getRegions(x))
      # fill missing countries
      x <- toolCountryFill(x,fill=0,verbisity=2)
      
      return(x)
}  
