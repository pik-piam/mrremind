#' read-in CSIRO electricity technology cost for Australia
#' @return magpie object of the CSIRO technology data
#' @author Felix Schreyer
#' @seealso \code{\link{readSource}}
#' @importFrom readxl read_excel

readCSIRO <- function() {

  
  CSIRO <- read_excel("CSIRO_TechCost.xlsx", sheet = 2) 
  CSIRO$scenario <- "2D"
  CSIRO$country <- "AUS"
  CSIRO$variable <- "CAPEX [2017AUSD/kW]"
  CSIRO <- CSIRO[,c(5,1,4,2,6,3)]
  
  x <- as.magpie(CSIRO, spatial = 1, temporal = 2, datacol = 6)

  return(x)
}  
