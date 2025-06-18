#' Calculate Emission Targets reference to be used when calculation Emission Targets
#'
#' Uses historical CEDS emissions from 1990-2015
#' CO2 (excl LU), CH4, N2O (so far no F-Gas historic time series)
# Note: CEDS2024 does not include 'Emi|N2O|Land Use|*' variables and cannot be used.
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Rahel Mandaroux, Falk Benke
#' @seealso [calcEmiTarget()], [convertUNFCCC_NDC()]
calcEmiTargetReference <- function() {
  ceds <- calcOutput("Emissions", datasource = "CEDS2REMIND", years = 1990:2015, aggregate = FALSE)

  # Global Warming Potentials of CH4 and N20, AR5 WG1 CH08 Table 8.7
  gwpCH4 <- 28
  gwpN2O <- 265

  # Calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq]
  n2Ovars <- c(
    "Emi|N2O|Energy and Industrial Processes (kt N2O/yr)",
    "Emi|N2O|Land Use|Agriculture and Biomass Burning (kt N2O/yr)",
    "Emi|N2O|Land Use|Forest Burning (kt N2O/yr)",
    "Emi|N2O|Land Use|Grassland Burning (kt N2O/yr)",
    "Emi|N2O|Waste (kt N2O/yr)"
  )

  ch4vars <- c(
    "Emi|CH4|Energy and Industrial Processes (Mt CH4/yr)",
    "Emi|CH4|Land Use|Agriculture and Biomass Burning (Mt CH4/yr)",
    "Emi|CH4|Land Use|Forest Burning (Mt CH4/yr)",
    "Emi|CH4|Land Use|Grassland Burning (Mt CH4/yr)",
    "Emi|CH4|Waste (Mt CH4/yr)"
  )

  ghg <- ceds[, , c("Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)")] +
    gwpN2O / 1000 * dimSums(ceds[, , n2Ovars], dim = 3) +
    gwpCH4 * dimSums(ceds[, , ch4vars], dim = 3)

  getNames(ghg) <- "GHG Total"

  return(list(
    x = ghg, unit = "Mt CO2eq",
    description = "historical total GHG emissions from 1990-2015 according to CEDS"
  ))
}
