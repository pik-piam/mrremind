#' @title convertGFED
#' @description Transforms GFED data on cellular level to country level
#' @param x MAgPIE object obatined from calcGFED()
#' @param subtype emissions or emissionfactors
#' @return magpie object of the GFED data in country level
#' @author Lavinia Baumstark, Abhijeet Mishra
#' @seealso \code{\link{readGFED}}
#' @examples
#' 
#' \dontrun{ x <- readGFED(subtype="emissions")
#' }
#' \dontrun{ y <- convertGFED(x)
#' }
#' @importFrom magclass getRegionList<-

convertGFED <- function(x, subtype) {
  if(subtype=="12regions_baseline"){
    # delete global information (how to deal with it?)
    x   <- x["GLO",,,invert=TRUE]
    #data(moinput)
    iso_country <- read.csv2(system.file("extdata","iso_country.csv",package = "madrat"),row.names=NULL)
    iso_country1<-as.vector(iso_country[,"x"])
    names(iso_country1)<-iso_country[,"X"]
    w   <- new.magpie(iso_country1,2005,getNames(x),fill=1)
    y <- toolAggregate(x, "regionmappingGFED.csv", weight=w)
    return(y)
  } else if(subtype=="emissions"){
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)
    countrylevel <- groupAggregate(x,query = mapping,from="celliso",to="iso",dim=1)
    countrylevel <- toolCountryFill(countrylevel,fill = 0)
    return(countrylevel)
  }  else {
    return(x)
  }
}  