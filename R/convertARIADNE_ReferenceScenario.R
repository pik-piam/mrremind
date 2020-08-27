
convertARIADNE_ReferenceScenario <- function(x, subtype){
  getRegions(x) <- countrycode(getRegions(x), 'iso2c', 'iso3c')
  x <- toolCountryFill(x)
  return(x)
}

