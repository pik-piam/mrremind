convertAmmoniaProductionUSGS <- function(x){
  
  usgstomagpie <-  read.csv("CountryUSGStoMAgPIE.csv", stringsAsFactors = FALSE)
 tmp <- getRegions(x)
    tmp[match(usgstomagpie$Country.USGS, tmp, nomatch = FALSE)] <- usgstomagpie$Country.MagPIE
    getRegions(x) <- tmp
  map <- toolMappingFile(type="regional", readcsv=T, name="regionmappingMAgPIE.csv")
  
  y <- toolAggregate(x,map, partrel=T, from=1, to=2)
  y <- toolCountryFill(y,fill = 0)
  y[is.na(y)] <- 0

  return(y)
}