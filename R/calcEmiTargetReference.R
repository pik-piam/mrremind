#' Calculate Emission Targets reference to be used when calculation Emission Targets
#'
#' Uses historical CEDS emissions from 1990-2023
#' CO2 (excl LU), CH4, N2O (so far no F-Gas historic time series)
#' @author Rahel Mandaroux, Falk Benke
#' @seealso [calcEmiTarget()], [convertUNFCCC_NDC()]
calcEmiTargetReference <- function() {
  ceds <- calcOutput("Emissions", datasource = "CEDS2025", years = 1990:2023, aggregate = FALSE)

  # Global Warming Potentials of CH4 and N20, AR5 WG1 CH08 Table 8.7
  gwpCH4 <- 28
  gwpN2O <- 265

  # Calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq]
  ghg <- dimSums(ceds[, , c("Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)",
                            "Emi|CO2|Agriculture (Mt CO2/yr)",
                            "Emi|CO2|Waste (Mt CO2/yr)")], dim = 3) +
    gwpN2O / 1000 * dimSums(ceds[, , c("Emi|N2O|Energy and Industrial Processes (kt N2O/yr)",
                                       "Emi|N2O|Agriculture (kt N2O/yr)",
                                       "Emi|N2O|Waste (kt N2O/yr)")], dim = 3) +
    gwpCH4 * dimSums(ceds[, , c("Emi|CH4|Energy and Industrial Processes (Mt CH4/yr)",
                                "Emi|CH4|Agriculture (Mt CH4/yr)",
                                "Emi|CH4|Waste (Mt CH4/yr)")], dim = 3)

  getNames(ghg) <- "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"

  return(list(
    x = ghg,
    unit = "Mt CO2eq",
    description = "historical total GHG emissions from 1990 to 2023 according to CEDS"
  ))
}
