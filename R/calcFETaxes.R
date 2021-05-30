#' Calculate FETaxes
#' 
#' Reads in the data of the source IIASA_subs_taxes, by country. and 
#' calculate taxes at the final energy delivery level to the end-use sectors
#' (industry, buildings and transport). Regional aggregation is done via the
#' respective energy quantities as weights.
#' 
#' @param subtype choose between tax rates ("taxes") or subsidies rate ("subsidies") output
#' 
#' @return MAgPIE object
#' @author Christoph Bertram and Renato Rodrigues
#' @seealso \code{\link{calcOutput}}, \code{\link{readIIASA_subs_taxes}},
#' \code{\link{convertIIASA_subs_taxes}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("FETaxes")
#' 
#' }
#' 
calcFETaxes <- function(subtype="taxes") {
  
  #read in taxes/subsidies values
  if (subtype == "taxes"){
    tax <- readSource("IIASA_subs_taxes", subtype="tax_rate")
    desc <- "Aggregated final energy tax data from country level data provided by IIASA (Jessica Jewell)"
  } else if (subtype == "subsidies"){
    tax <- -readSource("IIASA_subs_taxes", subtype="subsidies_bulk")
    desc <- "Aggregated final energy subsidy data from country level data provided by IIASA (Jessica Jewell)"
  } else {
    stop("the subtype must be either 'taxes' or 'subsidies'")
  }
  
  #read in energy values
  energy <- readSource("IIASA_subs_taxes", subtype="energy")
  #energy = 0 for regions/carriers with no information on subsidies, so that they are not considered in the weighting
  energy[is.na(tax)] <- 0
  #taxes without value are considered to be zero
  tax[is.na(tax)] <- 0
  #energy without value is considered to be zero
  energy[is.na(energy)] <- 0
  
  tax_map = list(
    "indst" = c(
      "fegas" = "IN-Naturalgas",
      "feh2s" = "IN-Naturalgas",
      "fehos" = "IN-Oil",
      "fesos" = "IN-Coal",
      "feels" = "IN-Electricity",
      "fehes" = "TRP-Oilproducts"
    ),
    "build" = c(
      "fegas" = "RC-Naturalgas",
      "feh2s" = "RC-Naturalgas",
      "fehos" = "RC-Heatingoil",
      "fesos" = "RC-Coal",
      "feels" = "RC-Electricity",
      "fehes" = "TRP-Oilproducts"
    ),
    "trans" = c(
      "fepet" = "TRP-Oilproducts",
      "fedie" = "TRP-Oilproducts",
      "fegat" = "TRP-Oilproducts",
      "feh2t" = "TRP-Oilproducts",
      "feelt" = "RC-Electricity"
    )
  )
  
 
  Rtax <- Renergy <- NULL
  for(sector in c("indst","build","trans")){
    Rtax <- mbind(Rtax,
                  add_dimension(setNames(tax[,,tax_map[[sector]]],names(tax_map[[sector]])), dim = 3.1, add = "sector", nm = sector))
    Renergy <- mbind(Renergy,
                  add_dimension(setNames(energy[,,tax_map[[sector]]],names(tax_map[[sector]])), dim = 3.1, add = "sector", nm = sector))
  }
  
  # convert original data from bulk values to subsidies rates for the case of subsidies
  if (subtype == "subsidies"){  
    Rtax <- Rtax/Renergy*1e9 #converting from billion$/GJ to $/GJ
    Rtax[is.na(Rtax)] <- 0
  }
  
  #Ex-post tax rate adjustments
  #feelt tax rate as weighted average on industry and buildings
  Rtax[,,"trans.feelt"] <- (Rtax[,,"indst.feels"]*Renergy[,,"indst.feels"] + Rtax[,,"build.feels"]*Renergy[,,"build.feels"]) / (Renergy[,,"indst.feels"] + Renergy[,,"build.feels"])
  Rtax[,,"trans.feelt"][is.na(Rtax[,,"trans.feelt"])] <- 0
  
  # disabling tax for fehes and feh2t 
  Rtax[,,c("feh2t","fehes")] <- 0
  # do not apply gas subsidies to H2
  if (subtype == "subsidies"){
    Rtax[,,c("feh2s")] <- 0
  }
  
  #Ex-post energy weight adjustments 
  Renergy[,,'build.feh2s'] <- Renergy[,,'build.fegas']
  Renergy[,,'indst.feh2s'] <- Renergy[,,'indst.fegas']
  Renergy[,,'trans.feelt'] <- Renergy[,,"build.feels"]
  Renergy[,,c("feh2t","fehes")] <- 0  

  #cdr sector taxes equal to industry taxes
  cdrTax <- Rtax[,,"indst"]
  getNames(cdrTax,dim = 1) <- "cdr"
  Rtax <- mbind(Rtax,cdrTax)
  cdrEnergy <- Renergy[,,"indst"]
  getNames(cdrEnergy,dim = 1) <- "cdr"
  Renergy <- mbind(Renergy,cdrEnergy)
  
  #bunkers sector taxes equal to transport taxes
  bunkersTax <- Rtax[,,"trans"]
  getNames(bunkersTax,dim = 1) <- "bunkers"
  Rtax <- mbind(Rtax,bunkersTax)
  bunkersEnergy <- Renergy[,,"trans"]
  getNames(bunkersEnergy,dim = 1) <- "bunkers"
  Renergy <- mbind(Renergy,bunkersEnergy)
  
  #set base year
  getYears(Rtax) <- "2005"
  getYears(Renergy) <- "2005"
  
  # Weights do not take into account the differentiation by services. So if the tax in a Cooling country
  # is very high and the tax in a country in the same region using a lot of electricity for cooking is low,
  # the tax for cooling and cooking with electricity will be equal where it should be high for cooling and low for cooking
  # So, we can assume that countries are app. similar in a given region

  return(list(x=Rtax,weight=Renergy,unit="$2005/GJ",description=desc))
}
