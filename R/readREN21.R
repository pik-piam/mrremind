#' Reads policy database from REN21 2017 with capacity targets or regional technology costs
#' @description Reads excel sheet with data on proposed policies,
#' on Renewable energy capacity targets (which are broken down into Total Installed Capacity
#' (TIC-Absolute), Additional Installed Capacity (AC-Absolute), and Production Absolute targets)
#' or regional technology costs
#' @details Country name is ISO coded. Capacity/Additional Capacity targets are in GW. Generation/Production targets are in GWh.
#' @return magpie object with Total Installed Capacity targets in GW for different target years
#' @author Aman Malik, Lavinia Baumstark
#' @param subtype Capacity Generation Emissions Share
#' @importFrom readxl read_excel

readREN21 <- function(subtype) {
  if (subtype == "Capacity") {
    REN21 <- read_excel("REN21.xlsx", sheet = "REN21")
    x <- as.magpie(REN21, spatial = 1, temporal = 2, datacol = 3)
  } else if (subtype == "investmentCosts") {
    REN21 <- read.csv("REN21_investment_costs.csv", sep = ";")
    x <- as.magpie(REN21, spatial = 1)
  }
  return(x)
}
