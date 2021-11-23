#' Converts Dylan's Australian gas cost to magpie
#' @param x MAgPIE object to be converted
#' @return magpie object of the CEMO data
#' @author Felix Schreyer
#' @seealso \code{\link{readSource}}




convertDylanAusGasCost <- function(x){
  
  
  all_c <- toolGetMapping("regionmappingH12.csv",where = "mappingfolder",type = "regional")

  # converting to 2005USD, conversion factor from 2015AUSD to 2005USD assumed 0.6 
  x_help <- x * 0.6
  x_help <- add_dimension(x_help, dim=3.4, add="unit", nm = "Natural Gas Extraction Cost [2005USD/GJ]")

  x = new.magpie(all_c$CountryCode, getYears(x_help), getNames(x_help), fill=0)
  x["AUS",,] <- x_help

  return(x)
}
