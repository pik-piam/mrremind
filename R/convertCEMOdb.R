#' Converts CEMO database data
#' 
#' @param x MAgPIE object to be converted
#' @return magpie object of the CEMO data
#' @author Felix Schreyer
#' @seealso \code{\link{readSource}}




convertCEMOdb <- function(x){
  

  all_c <- toolGetMapping("regionmappingH12.csv",where = "mappingfolder",type = "regional")

  # converting to 2015USD, conversion factor from 2015AUSD to 2015USD assumed 0.75 
  x_help <- x * 0.75
  getNames(x_help, dim = 3) = "CAPEX [2015USD/kW]" 
    
  x = new.magpie(all_c$CountryCode, getYears(x_help), getNames(x_help), fill=0)
  x["AUS",,] <- x_help

  return(x)
}
 

