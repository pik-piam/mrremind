#' convertRCP 
#' convert RCP data
#' 
#' @return magpie object of the RCP data
#' @author Julian Oeser
#' @param x Input object obtained by readSource
#' @param subtype Either 'Waste' or 'AviationShipping'
#'
#' @importFrom madrat toolAggregate


convertRCP <- function(x, subtype){
  
  if (subtype=="Waste") {
    
    # weights from EDGAR emissions
    ch4 <- readSource("EDGAR", subtype="ch4waste")
    so2 <- readSource("EDGAR", subtype="SO2")
    bc <- readSource("EDGAR", subtype="CO")
    oc <- readSource("EDGAR", subtype="CO")
    co <- readSource("EDGAR", subtype="CO")
    voc <- readSource("EDGAR", subtype="VOC")
    nh3 <- readSource("EDGAR", subtype="NH3")
    nox <- readSource("EDGAR", subtype="NOx")
    
    
    ch4 <- add_dimension(ch4, dim=3.1, "type", "ch4")
    so2 <- add_dimension(so2, dim=3.1, "type", "so2")
    bc <- add_dimension(bc, dim=3.1, "type", "bc")
    oc <- add_dimension(oc, dim=3.1, "type", "oc")
    co <- add_dimension(co, dim=3.1, "type", "CO")
    voc <- add_dimension(voc, dim=3.1, "type", "VOC")
    nh3 <- add_dimension(nh3, dim=3.1, "type", "nh3")
    nox <- add_dimension(nox, dim=3.1, "type", "NOx")
    
    weights <- mbind(ch4, so2, bc, oc, co, voc, nh3, nox)

    
    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingRCP.csv", returnPathOnly = TRUE, where = "mappingfolder")
    mapping <- read.csv2(mappingfile, stringsAsFactors = FALSE)
    countries <- mapping$CountryCode
    
    weights <- weights[countries,,]
    weights <- suppressWarnings(weights[,,"TOTAL", drop=TRUE])
    
    weights <- weights[,,getNames(x, dim="type")]

    out <- toolAggregate(x, mappingfile, weight = weights)
    
    out <- toolCountryFill(out, fill=0)
  
    
  } else if (subtype=="AviationShipping") {
    
     out <- x 
     
  } else {
    stop("Invalid subtype. Must be 'Waste' or 'AviationShipping'")
  }
  
  return(out)
}
