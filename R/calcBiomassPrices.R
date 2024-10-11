#' read biomass supply curves from Magpie emulator
#' @return Magpie object with two parameters determining linear biomass supply curve

calcBiomassPrices <- function() {
  "!# @monitor GDPuc::convertGDP"

  x <- readSource("MAgPIE", subtype = "supplyCurve_magpie_40")

  # rename the rcp-scenarios to cm_rcp_scen switches used in REMIND
  getNames(x) <- gsub("NDC-nocc_hist-PkBudg650",  "rcp20", getNames(x))
  getNames(x) <- gsub("NDC-nocc_hist-PkBudg1050", "rcp26", getNames(x))
  getNames(x) <- gsub("NDC-nocc_hist-NDC",        "rcp45", getNames(x))
  getNames(x) <- gsub("NPI-nocc_hist-Base",       "none", getNames(x))

  # rename SSP-scenarios to cm_LU_emi_scen switches used in REMIND
  getNames(x) <- gsub("SSP2EU", "SSP2", getNames(x))
  getNames(x) <- gsub("SDP-MC", "SDP",  getNames(x))

  # Introduce new SSP/SDP dimension by replacing "-" with "."
  getNames(x) <- gsub("(SSP[0-9]|SDP)-", "\\1.", getNames(x))
  getSets(x)["d3.1"] <- "scen1"
  getSets(x)["d3.2"] <- "scen2"
  getSets(x)["d3.3"] <- "char"

  # add supply curves for SSP3 using the curves from SSP2
  tmp <- x[, , "SSP2"]
  getNames(tmp) <- gsub("SSP2", "SSP3", getNames(tmp))
  x <- mbind(x, tmp)

  # if fit coefficients of a country are NA for all years (there is no supplycurve at all for this country)
  # generate artificial supplycurve with VERY high prices
  x[, , "a"][is.na(x[, , "a"])] <- 1
  x[, , "b"][is.na(x[, , "b"])] <- 0.1

  # apply conversion from US$2005 to US$2017 to "a" and "b", so that the price
  # calculated with these coefficients will be in US$2017
  x <- GDPuc::convertGDP(
    gdp = x,
    unit_in = "constant 2005 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )

  return(list(x           = x,
              weight      = calcOutput("FAOLand", aggregate = FALSE)[, , "6610", pmatch = TRUE][, "y2010", ],
              unit        = "none",
              description = "coefficients for the bioenergy supplycurve",
              aggregationFunction = toolBiomassSupplyAggregate))
}
