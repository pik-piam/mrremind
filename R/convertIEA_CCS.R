
convertIEA_CCS <- function(x) {
      
      # save Africa in seperate parameter
      x_AFR <- x["Africa",,]
      # split up Africa data into countries
      map   <- read.csv("regionmappingAfrica.csv",sep=";") 
      map$X <- toolCountry2isocode(map$X)
      w   <- calcOutput("Population",aggregate=FALSE)[map$X, 2005, "pop_SSP2"]
      x_AFR <- toolAggregate(x_AFR,map,weight=w)
      # delete Africa from x
      x <- x["Africa",,,invert=TRUE]
  
      # rename all other countries into ISO
      getRegions(x) <- toolCountry2isocode(getRegions(x))
      # add back countries of Africa
      x <- mbind(x,x_AFR)
      
      # fill missing countries
      x <- toolCountryFill(x,fill = 0)
      # set all NA to 0
      x[is.na(x)] <- 0
      
      return(x)
}  
