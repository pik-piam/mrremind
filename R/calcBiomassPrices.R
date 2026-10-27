#' read biomass supply curves from Magpie emulator
#' @return Magpie object with two parameters determining linear biomass supply curve

calcBiomassPrices <- function() {

  x <- readSource("MAgPIE", subtype = "supplyCurve_magpie_40")

  # create cm_LU_emi_scen names (as used in REMIND) from emulatpr scenario names

  # rename the rcp-scenarios to cm_rcp_scen switches used in REMIND
  getNames(x) <- gsub("PkBudg750",  "rcp20", getNames(x))
  getNames(x) <- gsub("PkBudg1000", "rcp26", getNames(x))
  getNames(x) <- gsub("NPi2025",    "rcp45", getNames(x))
  getNames(x) <- gsub("NDC",        "rcp37", getNames(x))

  # Move SSP to new dimension by replacing "-" with "."
  getNames(x) <- gsub("(SSP[0-9])-", "\\1.", getNames(x))
  getSets(x)["d3.1"] <- "ssp"
  getSets(x)["d3.2"] <- "rcp"
  getSets(x)["d3.3"] <- "char"

  # if fit coefficients of a country are NA for all years (there is no supplycurve at all for this country)
  # generate artificial supplycurve with VERY high prices
  x[, , "a"][is.na(x[, , "a"])] <- 1
  x[, , "b"][is.na(x[, , "b"])] <- 0.1

  list(x           = x,
       weight      = calcOutput("FAOLand", aggregate = FALSE)[, , "6610", pmatch = TRUE][, "y2010", ],
       unit        = "none",
       description = "coefficients for the bioenergy supplycurve",
       aggregationFunction = toolBiomassSupplyAggregate)
}
