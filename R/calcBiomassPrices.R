#' read biomass supply curves from Magpie emulator
#' @return Magpie object with two parameters determining linear biomass supply curve
#' @importFrom magclass dimReduce
#' @importFrom madrat readSource
#' @import mrcommons

calcBiomassPrices <- function(){
  
  x <- readSource("MAgPIE", subtype = "supplyCurve_magpie_40")

  # rename the rcp-scenarios
  # New scenarios (MAgPIE4.4.0), include nocc because climate change impacts are on by default)
  # TODO: Fix here when we have new emulators for other SSPs, currently only for SSP2
  # Remove all old SSP2 and SSP2EU scenarios # TODO: There's probably no need to read them in readMAgPIE in the first place
  x <- x[,,!(grepl("SSP2", getNames(x))&!grepl("nocc", getNames(x)))]
  # Copy SSP2 into SSP2EU
  xSSP2EU <- x[, , grepl("SSP2", getNames(x))]
  getNames(xSSP2EU) <- gsub("SSP2", "SSP2EU", getNames(xSSP2EU))
  x <- mbind(x, xSSP2EU)
  # Replace scenarios with nocc with the respective RCPs, do the old scenarios separately
  getNames(x) <- gsub("NDC-nocc-PkBudg500","rcp20", getNames(x)) 
  getNames(x) <- gsub("NDC-nocc-PkBudg1150","rcp26", getNames(x))
  getNames(x) <- gsub("NDC-nocc-NDC","rcp45", getNames(x))
  getNames(x) <- gsub("NPI-nocc-Base","none", getNames(x))
  # Remove unused nocc scenarios
  x <- x[,,!grepl("nocc", getNames(x))]


  # Old scenarios (MAgPIE <4.4.0), still using old budgets, TODO: replace as soon as we have new emulators
  getNames(x) <- gsub("NDC-PkBudg900","rcp20",getNames(x))
  getNames(x) <- gsub("NDC-PkBudg1300","rcp26",getNames(x))
  getNames(x) <- gsub("NDC-NDC","rcp45",getNames(x))
  getNames(x) <- gsub("NPI-Base","none",getNames(x))
  
  # Introduce new SSP/SDP dimension by replacing "-" with "."
  getNames(x) <- gsub("(SSP[0-9]|SSP2EU|SDP|SDP_EI|SDP_RC|SDP_MC)-","\\1.",getNames(x))
  
  # if fit coefficients of a country are NA for all years (there is no supplycurve at all for this country)
  # generate artificial supplycurve with VERY high prices
  x[,,"a"][is.na(x[,,"a"])] <- 1
  x[,,"b"][is.na(x[,,"b"])] <- 0.1
    

  return(list(x           = x,
              weight      = calcOutput("FAOLand", aggregate = F)[,,"6610",pmatch=TRUE][,"y2010",],
              unit        = "none",
              description = "coefficients for the bioenergy supplycurve",
              aggregationFunction=toolBiomassSupplyAggregate))
}
