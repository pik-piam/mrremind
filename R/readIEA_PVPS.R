#' Reads PV investment cost data for 2020 which are based on 2018 data from IEA PVPS report
#' @description reads excel sheet with PV investment cost data
#' @return  magpie object with PV investment cost data
#' @author  Felix Schreyer
readIEA_PVPS <- function() {
  IEA.PVPS <- readxl::read_excel("IEA_PVPS.xlsx", sheet = "IEA_PVPS_data")
  x <- as.magpie(IEA.PVPS, spatial = 1, datacol = 2)
  return(x)
}
