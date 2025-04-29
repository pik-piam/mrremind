#' Fugitive methane emissions from fossil fuel extraction
#' @description Fugitive methane emissions from fossil fuel extraction
#' @details REMIND uses historical data on fugitive methane emissions from fossil fuel extraction for coal, oil and gas
#' to derive emission factors. The data is available from two sources: EDGAR (for base year 2005) and CEDS2025 (2020).
#' @param source either "EDGAR" or "CEDS2025" (after REMIND ScenarioMIP release)
#' @return Magpie object with CH4 emissions from fossil fuel extraction for coal, oil and gas in 2005, in Mt CH4
#' @author Gabriel Abrahao
calcEmiFossilFuelExtr <- function(source) {
  if (source == "EDGAR") {
    year <- "y2005"

    data <- readSource("EDGAR", subtype = "ch4_history")[, year, c("1B1", "1B2")] / 1000
    getNames(data) <- c("coal", "oil_gas")

    # overwritting european countries with eurostat data
    EUcountries <- c("ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL", "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
    baselineEurostat <- calcOutput("HistEmissions", subtype = "MAC", aggregate = F)
    baselineEurostatSector <- calcOutput("HistEmissions", subtype = "sector", aggregate = F)
    data[EUcountries, 2005, "coal"] <- baselineEurostat[EUcountries, 2005, "ch4coal"]
    data[EUcountries, 2005, "oil_gas"] <- (setNames(baselineEurostatSector[EUcountries, 2005, "ch4.extraction.process"], nm = "oil_gas") - setNames(baselineEurostat[EUcountries, 2005, "ch4coal"], nm = "oil_gas"))

    # make new magpie-object
    x <- new.magpie(getRegions(data), year, c("pecoal", "peoil", "pegas"))
    # allocate coal
    x[, , "pecoal"] <- data[, , "coal"]

    # read in NIR-data for spit of oil and gas
    nir <- readSource("NIR", subtype = "1B2")[, year, ]
    # calculate global shares
    oil_glob <- dimSums(nir[, , "Oil"] + nir[, , "VentingOil"] + nir[, , "FlaringOil"], dim = 1) / dimSums(nir, dim = c(1, 3))
    gas_glob <- dimSums(nir[, , "Gas"] + nir[, , "VentingGas"] + nir[, , "FlaringGas"], dim = 1) / dimSums(nir, dim = c(1, 3))

    # split up coal and gas
    for (r in getRegions(x)) {
      if (!dimSums(nir[r, , ], dim = 3) == 0) {
        x[r, , "peoil"] <- data[r, , "oil_gas"] * (nir[r, , "Oil"] + nir[r, , "VentingOil"] + nir[r, , "FlaringOil"]) / dimSums(nir[r, , ], dim = 3)
        x[r, , "pegas"] <- data[r, , "oil_gas"] * (nir[r, , "Gas"] + nir[r, , "VentingGas"] + nir[r, , "FlaringGas"]) / dimSums(nir[r, , ], dim = 3)
      } else {
        x[r, , "peoil"] <- data[r, , "oil_gas"] * oil_glob
        x[r, , "pegas"] <- data[r, , "oil_gas"] * gas_glob
      }
    }
    # there is no time dimension in the GAMS code
    getYears(x) <- NULL

    return(list(
      x = x,
      weight = NULL,
      unit = "Mt CH4",
      description = "methane emissions in 2005"
    ))
  } else if (source == "CEDS2025") {
    year <- "y2020"

    categories <- c("1B1_Fugitive-solid-fuels", "1B2_Fugitive-petr",
                    "1B2b_Fugitive-NG-distr", "1B2b_Fugitive-NG-prod")
    indata <- readSource("CEDS2025")[, year, categories]
    indata <- indata[, , "ch4"]
    getNames(indata) <- c("pecoal", "peoil", "pegas_distr", "pegas_prod")

    # make new magpie-object
    x <- new.magpie(getRegions(indata), year, c("pecoal", "peoil", "pegas"))
    x[, , c("pecoal", "peoil")] <- indata[, , c("pecoal", "peoil")]
    x[, , c("pegas")] <- setNames(indata[, , "pegas_distr"] + indata[, , "pegas_prod"], c("pegas"))

    # there is no time dimension in the GAMS code
    getYears(x) <- NULL

    return(list(
      x = x,
      weight = NULL,
      unit = "Mt CH4",
      description = "methane emissions in 2020"
    ))
  } else {
    stop("Invalid source. Must be either 'EDGAR' or 'CEDS2025'.")
  }
}
