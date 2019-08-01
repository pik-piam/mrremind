

convertGlobalCCSinstitute <- function(x) {
 
    # sum over districts of a country
    y <- dimSums(x,dim=3)
    
    # transfer into ISO country names
    getRegions(y) <- toolCountry2isocode(getRegions(y))
    
    # fill all other countries with 0
    y <- toolCountryFill(y, fill = 0, verbosity=2)
  
  return(y)
}