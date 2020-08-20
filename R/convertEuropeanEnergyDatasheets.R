#' Convert European Energy Datasheets
#'
#' Converts European Energy Datasheets magpie object into appropriate form for the REMIND model
#' 
#' @param x European Energy Datasheets magpie object derived from readEuropeanEnergyDatasheets function
#' @return converted European Energy Datasheets magpie object
#' @author Renato Rodrigues and Atreya Shankar
#' @source European Energy Datasheets public database https://ec.europa.eu/energy/en/data-analysis/country 
#' @examples
#' \dontrun{test <- readSource("EuropeanEnergyDatasheets",convert=TRUE)}

convertEuropeanEnergyDatasheets <- function(x){
  iso3 <- read.csv2("isotwo2iso3Mapping.csv",stringsAsFactors = FALSE)
  getRegions(x) <- sapply(getRegions(x), function(y) iso3[which(iso3[,1] == y),2])
  # fill up zero countries
  x <- toolCountryFill(x)
  # removing international aviation from total emissions (REMIND reports total emissions without bunkers by default)
  x[,,"Emi|CO2 (Mt CO2/yr)"] <- x[,,"Emi|CO2 (Mt CO2/yr)"] - x[,,"Emi|CO2|Bunkers|International Aviation (Mt CO2/yr)"]
  x[,,"Emi|GHGtot (Mt CO2-equiv/yr)"] <- x[,,"Emi|GHGtot (Mt CO2-equiv/yr)"] - x[,,"Emi|GHG|Bunkers|International Aviation (Mt CO2-equiv/yr)"]
  return(x)
}