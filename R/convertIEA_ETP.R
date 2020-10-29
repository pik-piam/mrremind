#' Convert IEA ETP projections
#' 
#' @author Falk Benke
#' @param x IEA ETP projection magpie object derived from readIEA_ETP function
#' @param subtype data subtype. Either "main" or "industry_subsectors"
#' @importFrom madrat toolGetMapping

convertIEA_ETP <- function(x, subtype){
  
  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv", where="mappingfolder", type="regional")
  fe <- calcOutput("FE", source="IEA", aggregate=FALSE)
  
  if(subtype == 'industry_subsectors'){
    
    w <- fe[,2005,"FE|Transport (EJ/yr)"]
    x <- toolAggregate(x, regmapping, from="OECD", to="CountryCode", weight=w)
    
  } else {

    # make sure from-regions in mapping are identical to the regions of x
    m <- regmapping[regmapping$EEAReg != "rest",]
    # make sure the regions in weights are identical to the to-regions of the mapping
    w <- fe[m$CountryCode,2005,"FE|Transport (EJ/yr)"]
    x <- toolAggregate(x, m, from="EEAReg", to="CountryCode", weight=w)
  }
  
  x <- toolCountryFill(x)

  return(x)
}

