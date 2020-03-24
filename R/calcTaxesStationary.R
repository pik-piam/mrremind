#' Calculate TaxesStationary
#' 
#' Reads in the data of the source IIASA_subs_taxes, by country. Regional
#' aggregation is done via the respective energy quantities as weights.
#' 
#' @param sector taxes in transport or in the stationary/buildings_industry sector
#' @return MAgPIE object
#' @author Christoph Bertram
#' @seealso \code{\link{calcOutput}}, \code{\link{readIIASA_subs_taxes}},
#' \code{\link{convertIIASA_subs_taxes}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("TaxesStationary")
#' 
#' }
#' 
calcTaxesStationary <- function(sector) {
  
  #=== Check argument ====
  
  if (!(sector %in% c("transport","bit_st"))) stop("the argument sector must be in c('transport','bit_st')")
  
  #=======================
  
  #Define transport items and bit_st items
  transport = c("feelt","fepet","fedie","feh2t")
  bit_st = c("feels", "fehos", "fehes", "fegas", "fesos", "feh2s",
             "feeli", "fehoi", "fehei", "fegai", "fesoi", "feh2i",
             "feelb", "fehob", "feheb", "fegab", "fesob", "feh2b" )
  
  
  #produce the mapping. The stationary items are recomputed after. Including them in the mapping allows to create the category in the magclass object
  map_list = list(
    "IN-Naturalgas" = c("fegai", "fegas", 'feh2i', 'feh2s'),
    "IN-Oil" = c("fehoi", "fehos"),
    "IN-Coal" = c("fesoi","fesos"),
    "IN-Electricity" = c("feeli", "feels"),
    "RC-Naturalgas" = c("fegab", 'feh2b'),
    "RC-Heatingoil" = c("fehob"),
    "RC-Coal" = c("fesob"),
    "RC-Electricity" = c("feelb","feelt"),
    "TRP-Oilproducts" = c("fepet", "fedie","feh2t","fehes","feheb","fehei"))
  
  map = unlist(lapply(unique(names(map_list)), function(x){ tmp = map_list[[x]] ; names(tmp) = rep(x,length(tmp)); return(tmp)}))
  
  #========================
  
  #read in subsidy values
    tax             <- readSource("IIASA_subs_taxes", subtype="tax_rate")
    #read in energy values
    energy          <- readSource("IIASA_subs_taxes", subtype="energy")
    #energy = 0 for regions/carriers with no information on subsidies, so that they are not considered in the weighting
    energy[is.na(tax)] <- 0
    #taxes without value are considered to be zero
    tax[is.na(tax)] <- 0
    #energy without value is considered to be zero
    energy[is.na(energy)] <- 0
    
    
    #new variable with reduced 3rd dimension, aggregated Industry and ResidentialCommercial
    Rtax            <- tax[,,names(map)]
    Rtax            <- setNames(Rtax,map)
    
    #fegas is weighted average of IN-gas and RC-gas
    Rtax[,,"fegas"] <- (tax[,,"IN-Naturalgas"]*energy[,,"IN-Naturalgas"]+tax[,,"RC-Naturalgas"]*energy[,,"RC-Naturalgas"])/(energy[,,"IN-Naturalgas"]+energy[,,"RC-Naturalgas"])
    Rtax[,,'feh2s'] <- Rtax[,,'fegas']
    #fehos is weighted average of IN-oil and RC-Heatingoil
    Rtax[,,"fehos"] <- (tax[,,"IN-Oil"]*energy[,,"IN-Oil"]+tax[,,"RC-Heatingoil"]*energy[,,"RC-Heatingoil"])/(energy[,,"IN-Oil"]+energy[,,"RC-Heatingoil"])
    #fesos is weighted average of IN-coal and RC-coal
    Rtax[,,"fesos"] <- (tax[,,"IN-Coal"]*energy[,,"IN-Coal"]+tax[,,"RC-Coal"]*energy[,,"RC-Coal"])/(energy[,,"IN-Coal"]+energy[,,"RC-Coal"])
    #feels is weighted average of IN-Elec and RC-Elec
    Rtax[,,"feels"] <- (tax[,,"IN-Electricity"]*energy[,,"IN-Electricity"]+tax[,,"RC-Electricity"]*energy[,,"RC-Electricity"])/(energy[,,"IN-Electricity"]+energy[,,"RC-Electricity"])
    #feelt is equal to feels
    Rtax[,,"feelt"] <- Rtax[,,"feels"]
    #feh2s,feh2t,fehes = 0
    Rtax[,,c("feh2t","fehes","feheb","fehei")] <- 0
    
    #new variable with reduced 3rd dimension, aggregated Industry and ResidentialCommercial
    Renergy          <- energy[,,names(map)]
    Renergy          <- setNames(Renergy,map)
    
    # scale H2 taxes with natural gas
    Renergy[,,'feh2b'] <- energy[,,'RC-Naturalgas']
    Renergy[,,'feh2i'] <- energy[,,'IN-Naturalgas']
    Renergy[,,'feh2s'] <- Renergy[,,'feh2b'] + Renergy[,,'feh2i']
    
    Renergy[,,"fegas"] <- energy[,,"IN-Naturalgas"]+energy[,,"RC-Naturalgas"]
    Renergy[,,"fehos"] <- energy[,,"IN-Oil"]+energy[,,"RC-Heatingoil"]
    Renergy[,,"fesos"] <- energy[,,"IN-Coal"]+energy[,,"RC-Coal"]
    Renergy[,,"feels"] <- energy[,,"IN-Electricity"]+energy[,,"RC-Electricity"]
    Renergy[,,"feelt"] <- Renergy[,,"feelb"]
    Renergy[,,c("feh2t","fehes","feheb","fehei")] <- 0
    Rtax[is.na(Rtax)] <- 0
    getYears(Rtax) <- "2005"
    getYears(Renergy) <- "2005"
    
    
    #Reduce to the chosen sector
    
    if(sector == "transport") {
      Rtax = Rtax[,,transport]
      Renergy = Renergy[,,transport]
    } else if(sector == "bit_st"){
      Rtax = Rtax[,,bit_st]
      Renergy = Renergy[,,bit_st]
    }
    
    # Weights do not take into account the differentiation by services. So if the tax in a Cooling country
    # is very high and the tax in a country in the same region using a lot of electricity for cooking is low,
    # the tax for cooling and cooking with electricity will be equal where it should be high for cooling and low for cooking
    # So, we can assume that countries are app. similar in a given region
    return(list(x=Rtax,weight=Renergy,unit="$2005/GJ",description="Aggregated final energy tax data from country level data provided by IIASA (Jessica Jewell)"))
# energy <- energy[,,getNames(tax)]
#   return(list(x=tax,weight=energy))
#   return(list(x=energy, weight=NULL))
}
