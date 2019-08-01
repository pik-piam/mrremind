#' Calculate SubsStationary
#' 
#' Reads in the data of the source IIASA_subs_taxes, by country. Regional
#' aggregation is done via the respective energy quantities as weights.
#' 
#' @param sector subsidies in transport or in the stationary/buildings_industry sector
#' @return MAgPIE object
#' @author Christoph Bertram
#' @seealso \code{\link{calcOutput}}, \code{\link{readIIASA_subs_taxes}},
#' \code{\link{convertIIASA_subs_taxes}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("SubsStationary")
#' 
#' }
#' 
calcSubsStationary <- function(sector) {
  
  #=== Check argument ====
  
  if (!(sector %in% c("transport","bit_st","extraction"))) stop("the argument sector must be in c('transport','bit_st','extraction')")
  
  #=======================
  
  #Define transport items and bit_st items
  transport = c("feelt","fepet","fedie","feh2t")
  bit_st = c("feels", "fehos", "fehes", "fegas", "fesos", "feh2s",
             "feeli", "fehoi", "fehei", "fegai", "fesoi", "feh2i",
             "feelb", "fehob", "feheb", "fegab", "fesob", "feh2b")
  extraction = c("pecoal","pegas","peoil")
  
  #produce the mapping. The stationary items are recomputed after. Including them in the mapping allows to create the category in the magclass object
  #same for heat and hydrogen in TRP-Oilproducts
  map_list = list(
    "PE-Naturalgas" = "pegas",
    "PE-Oil" = "peoil",
    "PE-Coal" = "pecoal",
    "IN-Naturalgas" = c("fegai", "fegas"),
    "IN-Oil" = c("fehoi", "fehos"),
    "IN-Coal" = c("fesoi","fesos"),
    "IN-Electricity" = c("feeli", "feels"),
    "RC-Naturalgas" = c("fegab"),
    "RC-Heatingoil" = c("fehob"),
    "RC-Coal" = c("fesob"),
    "RC-Electricity" = c("feelb","feelt"),
    "TRP-Oilproducts" = c("fepet", "fedie", "feh2s","feh2t","fehes","feheb","fehei","feh2b","feh2i"))
  
  map = unlist(lapply(unique(names(map_list)), function(x){ tmp = map_list[[x]] ; names(tmp) = rep(x,length(tmp)); return(tmp)}))
  
  #========================
  
  #read in bulk subsidy values
  sub             <- -readSource("IIASA_subs_taxes", subtype="subsidies_bulk")
  #read in energy values
  energy          <- readSource("IIASA_subs_taxes", subtype="energy")
  #reduce energy set to carriers that have a subsidy value
  energy <- energy[,,getNames(sub)]
  #energy = 0 for regions/carriers with no information on subsidies, so that they are not considered in the weighting
  energy[is.na(sub)] <- 0
  #subsidies without value are considered to be zero
  sub[is.na(sub)] <- 0
  #energy without value is considered to be zero
  energy[is.na(energy)] <- 0
  
  #new variable for rates, with expanded 3rd dimension, both disaggregated for Industry and RC, as well as aggregated Industry and ResidentialCommercial
  
  Rsub            <- sub[,,names(map)]/energy[,,names(map)]*1e9 #converting from billion$/GJ to $/GJ
  Rsub            <- setNames(Rsub,map)
  Rsub            <- Rsub
  #fegas is weighted average of IN-gas and RC-gas
  Rsub[,,"fegas"] <- (sub[,,"IN-Naturalgas"]+sub[,,"RC-Naturalgas"])/(energy[,,"IN-Naturalgas"]+energy[,,"RC-Naturalgas"])*1e9
  #fehos is weighted average of IN-oil and RC-Heatingoil
  Rsub[,,"fehos"] <- (sub[,,"IN-Oil"]+sub[,,"RC-Heatingoil"])/(energy[,,"IN-Oil"]+energy[,,"RC-Heatingoil"])*1e9
  #fesos is weighted average of IN-coal and RC-coal
  Rsub[,,"fesos"] <- (sub[,,"IN-Coal"]+sub[,,"RC-Coal"])/(energy[,,"IN-Coal"]+energy[,,"RC-Coal"])*1e9
  #feels is weighted average of IN-Elec and RC-Elec
  Rsub[,,"feels"] <- (sub[,,"IN-Electricity"]+sub[,,"RC-Electricity"])/(energy[,,"IN-Electricity"]+energy[,,"RC-Electricity"])*1e9
  #feelt is equal to feels
  Rsub[,,"feelt"] <- Rsub[,,"feels"]
  #feh2s,feh2t,fehes = 0
  Rsub[,,c("feh2s","feh2t","fehes","feheb","fehei","feh2b","feh2i")] <- 0
  
  
  #new variable with reduced 3rd dimension, aggregated Industry and ResidentialCommercial
  
  Renergy          <- energy[,,names(map)]
  Renergy          <- setNames(Renergy,map)
  Renergy[,,"fegas"] <- energy[,,"IN-Naturalgas"]+energy[,,"RC-Naturalgas"]
  Renergy[,,"fehos"] <- energy[,,"IN-Oil"]+energy[,,"RC-Heatingoil"]
  Renergy[,,"fesos"] <- energy[,,"IN-Coal"]+energy[,,"RC-Coal"]
  Renergy[,,"feels"] <- energy[,,"IN-Electricity"]+energy[,,"RC-Electricity"]
  Renergy[,,"feelt"] <- Renergy[,,"feelb"]
  Renergy[,,c("feh2s","feh2t","fehes","feheb","fehei","feh2b","feh2i")] <- 0
  Rsub[is.na(Rsub)] <- 0
  getYears(Rsub) <- "2005"
  getYears(Renergy) <- "2005"
  
  #Reduce to the chosen sector
  
  if(sector == "transport") {
    Rsub = Rsub[,,transport]
    Renergy = Renergy[,,transport]
  } else if(sector == "bit_st"){
    Rsub = Rsub[,,bit_st]
    Renergy = Renergy[,,bit_st]
  } else if(sector == "extraction"){
    Rsub = Rsub[,,extraction]
    Renergy = Renergy[,,extraction]
  }
  
  
  return(list(x=Rsub,weight=Renergy,unit="$2005/GJ",description="Aggregated final energy subsidy data from country level data provided by IIASA (Jessica Jewell)"))
  energy <- energy[,,getNames(sub)]
  return(list(x=sub,weight=energy))
  return(list(x=energy,weight=NULL))
}
