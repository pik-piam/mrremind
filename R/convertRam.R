#' Regional multiliers from Ram et al. , 2018
# x <- readSource("Ram",convert=F)
#' @param x MAgPIE object to be converted

convertRam <- function(x){
  mapr <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder")
  
  #x <- readSource(type = "Ram",convert=F)
  x_final <- new.magpie(cells_and_regions = mapr$CountryCode,years = seq(2015,2050,5),names = "value")
  
  # mapping REMIND regions to regions from Ram et al.
  
  x_final[mapr[mapr$RegionCode=="EUR",]$CountryCode,,] <- x["Europe",,]
  x_final[mapr[mapr$RegionCode=="NEU",]$CountryCode,,] <- x["Europe",,]
  x_final[mapr[mapr$RegionCode=="LAM",]$CountryCode,,] <- x["South America",,]
  x_final[mapr[mapr$RegionCode=="MEA",]$CountryCode,,] <- x["MENA",,]
  x_final[mapr[mapr$RegionCode=="SSA",]$CountryCode,,]  <- x["Sub-Saharan Africa",,]
  x_final[mapr[mapr$RegionCode=="CHA",]$CountryCode,,] <- x["Northeast Asia",,]
  x_final[mapr[mapr$RegionCode=="JPN",]$CountryCode,,] <- x["Northeast Asia",,]
  x_final[mapr[mapr$RegionCode=="REF",]$CountryCode,,] <- x["Eurasia",,]
  x_final[mapr[mapr$RegionCode=="OAS",]$CountryCode,,] <- x["Southeast Asia",,]
  x_final[mapr[mapr$RegionCode=="USA",]$CountryCode,,] <- x["North America",,]
  x_final[mapr[mapr$RegionCode=="OAS",]$CountryCode,,] <- x["Southeast Asia",,]
  x_final[mapr[mapr$RegionCode=="CAZ",]$CountryCode,,] <- x["North America",,]
  x_final[c("IND","PAK","BGD","NPL","LKA","MDV","AFG","BTN"),,] <- x["SAARC",,]
  x <- x_final
  return (x)

}
