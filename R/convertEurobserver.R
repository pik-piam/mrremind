#' @title convertEurobserver 
#' @details Shares of world export for EU28 + some other countries for solar PV, wind, 
#' and hydropower AND employment data for EU28 countries for all RES techs. Don't use 2018 values.
#' @author Aman Malik
#' @param x magpie object
#' @param subtype wither "share" or "employment"
convertEurobserver <- function(x,subtype){
  
  if (subtype=="share"){
    
    #x <- readSource("Eurobserver",subtype = "share",convert = FALSE)
    x_row <- x["Rest of the world",,]
    x <- x["Rest of the world",,invert=T]
    x[is.na(x)] <- 0
    getRegions(x) <- gsub(pattern = "Luxemburg",replacement = "Luxembourg",getRegions(x))
    getRegions(x) <- toolCountry2isocode(getRegions(x))
    
    mapping <- toolMappingFile(type = "regional",name = "regionmappingREMIND.csv",readcsv = T,where = "mappingfolder")
    
    
    # indicator to distribute to ROW
    gdppp <-  calcOutput("GDPppp", years=2015,aggregate = F) 
    gdppp <- gdppp[,,"gdp_SSP2"]
    gdppp <- gdppp[getRegions(x),,invert=T]
    x_rem <- new.magpie(setdiff(mapping$CountryCode,getRegions(x)),getYears(x),getNames(x),fill = 1)
    x_rem <- x_rem*as.numeric(gdppp/dimSums(gdppp,dim = 1))
    for (i in getYears(x_rem)){
      for (j in getNames(x_rem)){
        x_rem[,i,j] <- x_rem[,i,j]*as.numeric(x_row[,i,j])
      }
    }
    
    x <- mbind(x,x_rem)
  }
    
    if (subtype=="employment")
    {
      #x <- readSource("Eurobserver",subtype = "employment",convert = FALSE)
      getRegions(x) <- toolCountry2isocode(getRegions(x))
      x <- toolCountryFill(x,fill=NA,verbosity = 1)
      
    }
    
   
  return (x)
}
