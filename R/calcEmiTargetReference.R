#' Calculate Emission Targets reference to be used when calculation Emission Targets
#'
#' Uses historical CEDS emissions from 1990-2023
#' CO2 (excl LU), CH4, N2O (so far no F-Gas historic time series)
#' @author Rahel Mandaroux, Falk Benke
#' @seealso [calcEmiTarget()], [convertUNFCCC_NDC()]

calcEmiTargetReference <- function() {
  # TODO Rahel: adjust documentation

  ceds <- calcOutput("Emissions", datasource = "CEDS2025", years = 1990:2022, aggregate = FALSE)
  unfccc <- collapseDim(calcOutput("UNFCCC", years = 1990:2022, aggregate = FALSE))
  unfcccReg <- intersect(getItems(unfccc, dim = 1), getItems(readSource("UNFCCC", convert = F), dim = 1))

  # Global Warming Potentials of CH4 and N20, AR5 WG1 CH08 Table 8.7
  gwpCH4 <- 28
  gwpN2O <- 265

  # calculate CEDS values ----

  # Calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq] without landuse
  GHGwoLULUCF <- dimSums(ceds[, , c(
    "Emi|CO2|w/o Bunkers|Energy and Industrial Processes (Mt CO2/yr)",
    "Emi|CO2|Agriculture (Mt CO2/yr)",
    "Emi|CO2|Waste (Mt CO2/yr)"
  )], dim = 3) +
    gwpN2O / 1000 * dimSums(ceds[, , c(
      "Emi|N2O|w/o Bunkers|Energy and Industrial Processes (kt N2O/yr)",
      "Emi|N2O|Agriculture (kt N2O/yr)",
      "Emi|N2O|Waste (kt N2O/yr)"
    )], dim = 3) +
    gwpCH4 * dimSums(ceds[, , c(
      "Emi|CH4|w/o Bunkers|Energy and Industrial Processes (Mt CH4/yr)",
      "Emi|CH4|Agriculture (Mt CH4/yr)",
      "Emi|CH4|Waste (Mt CH4/yr)"
    )], dim = 3)

  getNames(GHGwoLULUCF) <- "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"

  EmiLULUCF <- unfccc[, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]

  ghgCEDS <- mbind(GHGwoLULUCF, EmiLULUCF)

  ghgCEDS <- add_columns(ghgCEDS, "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)", dim = 3.1)
  ghgCEDS[, , "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] <-
    ghgCEDS[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"] +
    ghgCEDS[, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]


  # get UNFCCC values ----

  ghgUNFCCC <- unfccc[, , c(
    "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)",
    "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"
  )]

  ghgUNFCCC <- add_columns(ghgUNFCCC, "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)", dim = 3.1)

  ghgUNFCCC[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"] <-
    ghgUNFCCC[, , "Emi|GHG|w/o Bunkers|LULUCF national accounting (Mt CO2eq/yr)"] -
    ghgUNFCCC[, , "Emi|GHG|Land-Use Change|LULUCF national accounting (Mt CO2eq/yr)"]

  out <- ghgCEDS
  out[unfcccReg, , ] <- ghgUNFCCC[unfcccReg, , ]

  return(list(
    x = out,
    unit = "Mt CO2eq",
    # TODO Rahel: adjust description
    description = glue::glue("historical total GHG emissions from 1990 to 2023 \\
                             according to CEDS and GHG LULUCF from UNFCCC")
  ))
}
