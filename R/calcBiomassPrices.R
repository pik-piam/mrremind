#' read biomass supply curves from Magpie emulator
#' @return Magpie object with two parameters determining linear biomass supply curve
#' @importFrom magclass dimReduce
#' @importFrom madrat readSource
#' @import mrcommons

calcBiomassPrices <- function(){
  
  x <- readSource("MAgPIE", subtype = "supplyCurve_magpie_40")

  # rename the rcp-scenarios
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
              weight      = NULL,
              unit        = "none",
              description = "coefficients for the bioenergy supplycurve",
              aggregationFunction=toolBiomassSupplyAggregate))
}
