#' @importFrom madrat toolGetMapping
#' 
convertIEA_ETP <- function(x, subtype){
  
  # only focus on regions BRA, EUR, USA, CHN, IND, RUS, MEX, ZAF, ASEAN
  x <- x[c("Global", "OECD", "Non-OECD"),,, invert=T]

  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv", where="mappingfolder", type="regional")
  # make sure from-regions in mapping are identical to the regions of x
  m <- regmapping[regmapping$EEAReg != "rest",]

  fe <- calcOutput("FE", source="IEA", aggregate=FALSE)
  # make sure the regions in weights are identical to the to-regions of the mapping
  w <- fe[m$CountryCode,2005,"FE|Transport (EJ/yr)"]
  
  x <- toolAggregate(x, m, from="EEAReg", to="CountryCode", weight=w)
  x <- toolCountryFill(x)
  return(x)
}

