#' Reads shares of world manufacture for spv modules and wind turbines.
#' @author Aman Malik

convertProdShares <- function(){
  
  # disaggregating European countries
  mapping <- toolGetMapping("regionmappingH12.csv",type = "regional",where = "mappingfolder")
  mapping <- mapping[mapping$RegionCode=="EUR",]
  mapping$RegionCode <- "Europe"
  
  wt= calcOutput(type = "GDPppp",aggregate = F)[mapping$CountryCode,2020,"gdp_SSP2"]
  x2 <- toolAggregate(x = x["Europe","y2018","spv"],rel = mapping,weight = wt,from = "CountryCode",to = "RegionCode")
  
  y <- x[c("Europe","Other"),,invert=T]
  getRegions(y) <- toolCountry2isocode(getRegions(y))
  
  z <- new.magpie(base::union(getRegions(x2),getRegions(y)),years = getYears(x),names = getNames(x))
  z[getRegions(y),,] <- y 
  z[getRegions(x2),"y2018","spv"] <- x2
 
  x <- z
  x <- toolCountryFill(y,fill=0)
  
  return (x)
  
}
