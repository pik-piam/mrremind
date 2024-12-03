#' read biomass supply curves from Magpie emulator
#' @return Magpie object with two parameters determining linear biomass supply curve

calcBiomassPrices <- function() {

  x <- readSource("MAgPIE", subtype = "supplyCurve_magpie_40")

  # add supply curves for SDP-MC-SSP1-PkBudg1000 copying SDP-MC-SSP1-PkBudg650
  tmp <- x[, , "SDP-MC-SSP1-PkBudg650"] 
  getNames(tmp) <- gsub("SDP-MC-SSP1-PkBudg650", "SDP-MC-SSP1-PkBudg1000", getNames(tmp))
  x <- mbind(x, tmp)

  # create cm_LU_emi_scen names (as used in REMIND) from emulatpr scenario names
  # in SSP2-SSP2_lowEn-PkBudg650
  # - the first SSP-scenario ("SSP2") refers to the MAgPIE scenario selected for the emulator runs
  # - the second SSP-scenario ("SSP2_lowen") refers to the REMIND scenario the GHG prices were taken from
  # They need to be combined into a new scenario name that is used in REMIND's cm_LU_emi_scen
  getNames(x) <- gsub("SDP-MC-SSP1-",     "SSP1-",       getNames(x))
  getNames(x) <- gsub("SSP2-SSP2-",       "SSP2-",       getNames(x))
  getNames(x) <- gsub("SSP2-SSP2_lowEn-", "SSP2_lowEn-", getNames(x))
  getNames(x) <- gsub("SSP3-SSP2-",       "SSP3-",       getNames(x))
  getNames(x) <- gsub("SSP5-SSP5-",       "SSP5-",       getNames(x))

  # rename the rcp-scenarios to cm_rcp_scen switches used in REMIND
  getNames(x) <- gsub("PkBudg650",  "rcp20", getNames(x))
  getNames(x) <- gsub("PkBudg1000", "rcp26", getNames(x))
  getNames(x) <- gsub("NPi",        "rcp45", getNames(x))

  # Move SSP to new dimension by replacing "-" with "."
  getNames(x) <- gsub("(SSP[0-9](_lowEn|))-", "\\1.", getNames(x))
  getSets(x)["d3.1"] <- "ssp"
  getSets(x)["d3.2"] <- "rcp"
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
