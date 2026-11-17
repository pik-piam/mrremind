#' Disaggregates ScenarioMIP R10 F-gas emissions to countries
#'
#' Each gas species is distributed to the countries of its R10 region proportionally to the country emissions of
#' the same species in the base year 2020 of [readEDGAR7Fgases()] (as in [convertIMAGE2025()], but per species).
#' The weights are computed on the fly from `readSource("EDGAR7Fgases")`. The species are listed in the table
#' `readSource("ScenarioMIP", "FGasSpecies", convert = FALSE)`. Before that, the data of each model is interpolated
#' linearly to every year from 2020 to 2100. The assumptions are explained in the code.
#'
#' @param x magpie object from [readScenarioMIP()]
#' @param subtype "FGasScen" (the table subtype "FGasSpecies" is not country-level and is not converted)
#' @return magpie object at country level
#' @author Gabriel Abrahao
convertScenarioMIP <- function(x, subtype = "FGasScen") {
  if (subtype != "FGasScen") {
    stop("Only the subtype 'FGasScen' can be converted; read '", subtype, "' with convert = FALSE")
  }
  map <- toolGetMapping("regionmappingR10.csv", type = "regional", where = "mrremind")

  # Each model reports different years, and the object has all of them, with empty cells where a model has no value.
  # time_interpolate does not fill across empty cells, so each model is interpolated alone, on the years it reports,
  # linearly for every year from 2020 to 2100 (per region, scenario and variable). This is done on the regions, before
  # the emissions are distributed to the countries
  # MESSAGEix reports every year of 2020-2025, then every 5 years until 2060 and every 10 years afterwards
  xMESSAGEix <- x[, c(2020:2025, seq(2030, 2060, 5), seq(2070, 2100, 10)), "MESSAGEix"]
  xMESSAGEix <- time_interpolate(xMESSAGEix, 2020:2100, integrate_interpolated_years = TRUE)
  # IMAGE reports every 5 years until 2050 and every 10 years afterwards
  xIMAGE <- x[, c(seq(2020, 2050, 5), seq(2060, 2100, 10)), "IMAGE"]
  xIMAGE <- time_interpolate(xIMAGE, 2020:2100, integrate_interpolated_years = TRUE)
  x <- mbind(xMESSAGEix, xIMAGE)

  # The species table links each scenario variable to the EDGAR7 variable of the same gas. Variables of the scenario
  # that are not in the table (the total "Emissions|F-Gases" in Mt CO2eq, which calcFGas rebuilds from the species) are
  # dropped here, because they are not distributed to the countries
  sp <- readSource("ScenarioMIP", subtype = "FGasSpecies", convert = FALSE)
  sp <- sp[sp$smip_variable %in% getItems(x, dim = "variable"), ]

  # The countries of an R10 region share its emissions of a gas in proportion to their EDGAR7 emissions of the same gas
  # in 2020. Assumptions:
  #  - the shares of 2020 are kept constant for all years up to 2100 and for all scenarios
  #  - a tiny constant is added to the emissions, so that a region where EDGAR7 has zero emissions of a gas in all
  #    countries is split equally between its countries, instead of weights that sum to zero (toolAggregate would fail
  #    or return NaN)
  eps <- 1e-10
  edgar <- readSource("EDGAR7Fgases")[, 2020, sp$remind_variable] + eps

  # Each gas is distributed with its own weight. Dropping the single year (2020) leaves the weight with countries only,
  # which is what toolAggregate needs
  mbind(lapply(seq_len(nrow(sp)), function(i) {
    weight <- collapseDim(edgar[, , sp$remind_variable[i]])
    toolAggregate(x[, , sp$smip_variable[i]], map, weight = weight, from = "RegionCode", to = "CountryCode")
  }))
}
