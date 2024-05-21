#' Reads PV investment cost data for 2020 which are based on 2018 data from IEA PVPS report
#' @description reads excel sheet with PV investment cost data 
#' @return  magpie object with PV investment cost data
#' @author  Felix Schreyer
#' @param subtype type of data read from IEA PVPS
#' @importFrom readxl read_excel


readIEA_PVPS <- function(subtype){
  if (subtype == "CAPEX") {
    IEA.PVPS <- read_excel("IEA_PVPS.xlsx", sheet = "IEA_PVPS_data") 
    x <- collapseNames(as.magpie(IEA.PVPS,spatial=1,datacol=2))
  } 
  return(x)
}
