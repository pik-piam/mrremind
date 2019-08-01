#' read biomass supply curves from Magpie emulator
#' @return Magpie object with two parameters determining linear biomass supply curve
#' @importFrom magclass dimReduce

calcBiomassPrices <- function(){

  x <- readSource("MAgPIE", subtype = "supplyCurve_magpie_40")
  
  # rename the rcp-scenarios
  getNames(x) <- gsub("NDC-Budg600","rcp20",getNames(x))
  getNames(x) <- gsub("NDC-Budg1300","rcp26",getNames(x))
  getNames(x) <- gsub("NDC-NDC","rcp45",getNames(x))
  getNames(x) <- gsub("BASE-Base","none",getNames(x))
  
  # Introduce new SSP dimension by replacing "-" with "."
  getNames(x) <- gsub("(SSP[0-9])-","\\1.",getNames(x))
  
  ### Australia ###
    # take USA biomass supply curves for Australia (preliminary fix)
  x["AUS",,"a"] <- dimReduce(x["USA",,"a"])
  x["AUS",,"b"] <- dimReduce(x["USA",,"b"]) 
  
  # take RUS biomass supply curves for Canada
  x["CAN",,"a"] <- dimReduce(x["RUS",,"a"])
  x["CAN",,"b"] <- dimReduce(x["RUS",,"b"]) 
  
  
    
  # if fit coefficients of a country are NA for all years (there is no supplycurve at all for this country) 
  # generate artificial supplycurve with VERY high prices
  x[,,"a"][is.na(x[,,"a"])] <- 1
  x[,,"b"][is.na(x[,,"b"])] <- 0.1
    

  
  # use total land area as weight
  w <- calcOutput("LanduseInitialisation",aggregate=FALSE)[,2005,] 
  # sum over 3.dimension
  w <- dimSums(w,dim=3)
  getYears(w) <- NULL
  
  return(list(x           = x,
              weight      = w,
              unit        = "none",
              description = "coefficients for the bioenergy supplycurve"))
}