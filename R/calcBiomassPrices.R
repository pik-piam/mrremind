#' read biomass supply curves from Magpie emulator
#' @return Magpie object with two parameters determining linear biomass supply curve

calcBiomassPrices <- function() {

  x <- readSource("MAgPIE", subtype = "supplyCurve_magpie_40")
  
  # add supply curves for SSP1-PkBudg1000 copying SSP1-PkBudg650
  tmp <- x[, , "SSP1-SSP1-PkBudg650"]
  getNames(tmp) <- gsub("SSP1-SSP1-PkBudg650", "SSP1-SSP1-PkBudg1000", getNames(tmp))
  x <- mbind(x, tmp)

  # rename the rcp-scenarios to cm_rcp_scen switches used in REMIND
  # eample: SSP2-SSP2_lowEn-PkBudg1000 -> SSP2-rcp26
  getNames(x) <- gsub("SSP[1-5](_lowEn|)-PkBudg650",  "rcp20", getNames(x))
  getNames(x) <- gsub("SSP[1-5](_lowEn|)-PkBudg1000", "rcp26", getNames(x))
  getNames(x) <- gsub("SSP[1-5](_lowEn|)-NPi",        "rcp45", getNames(x))

  # Introduce new SSP/SDP dimension by replacing "-" with "."
  getNames(x) <- gsub("(SSP[0-9]|SDP)-", "\\1.", getNames(x))
  getSets(x)["d3.1"] <- "scen1"
  getSets(x)["d3.2"] <- "scen2"
  getSets(x)["d3.3"] <- "char"

  # if fit coefficients of a country are NA for all years (there is no supplycurve at all for this country)
  # generate artificial supplycurve with VERY high prices
  x[, , "a"][is.na(x[, , "a"])] <- 1
  x[, , "b"][is.na(x[, , "b"])] <- 0.1

  return(list(x           = x,
              weight      = calcOutput("FAOLand", aggregate = FALSE)[, , "6610", pmatch = TRUE][, "y2010", ],
              unit        = "none",
              description = "coefficients for the bioenergy supplycurve",
              aggregationFunction = toolBiomassSupplyAggregate))
}
