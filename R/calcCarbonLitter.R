#' @title calcCarbonLitter
#' @description Calculates Carbon Input from litter for land use types.
#' 
#' @param landuse_types Sets: carbon or ini_new (magpie initialisation)
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' 
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CarbonLitter")
#' }
#' @importFrom magclass dimCode

calcCarbonLitter <- function(landuse_types= "carbon", cellular=TRUE){
  
  if(!cellular)(stop("For the moment just on cellular level"))
  
  years   <- findset("past")
  Landuse <- calcOutput("Landuse", landuse_types=landuse_types, cellular=cellular, aggregate=FALSE)[,years,]
  
  ##################
  ### single     ###
  ##################
  
  LPJml2MAgPIE         <- c(past = "lu_grass", natveg = "nat_veg")     
  LitterRespiration_ha <- mbind(calcOutput("LPJmlCarbon", landtype=LPJml2MAgPIE["past"], subtype="mrh_litter", aggregate=FALSE),
                                calcOutput("LPJmlCarbon", landtype=LPJml2MAgPIE["natveg"], subtype="mrh_litter", aggregate=FALSE))
      
  Respiration2Decomposition <- 10/7 # 70% of decomposed litter is respired (reverse logic is used to calculate total decomposed litter) 
  UptakeRateLitter          <- 3/10 # 30% of decomposed litter is taken up by the soil
  CarbonFlux2Soil_ha        <- LitterRespiration_ha * Respiration2Decomposition * UptakeRateLitter
  
  
  ##################
  ### total set  ###
  ##################
  if(landuse_types=="carbon"){
    
    complete_litter     <- c("urban","crop")
    CarbonFlux2Soil_ha  <- add_columns(CarbonFlux2Soil_ha, addnm=complete_litter, dim = dimCode("landuse", CarbonFlux2Soil_ha))
    CarbonFlux2Soil_ha[,,complete_litter] <- 0 
    
  } else if(landuse_types=="ini_new"){
    
    complete_litter     <- c("urban","crop")
    CarbonFlux2Soil_ha  <- add_columns(CarbonFlux2Soil_ha, addnm=complete_litter, dim = dimCode("landuse", CarbonFlux2Soil_ha))
    CarbonFlux2Soil_ha[,,complete_litter] <- 0 
    
    natveg_disagg       <- c("primforest","secdforest","forestry","other")
    CarbonFlux2Soil_ha  <- add_columns(CarbonFlux2Soil_ha, addnm=natveg_disagg, dim = dimCode("landuse", CarbonFlux2Soil_ha))
    CarbonFlux2Soil_ha[,,natveg_disagg] <- CarbonFlux2Soil_ha[,,"natveg"]
    CarbonFlux2Soil_ha  <- CarbonFlux2Soil_ha[,,"natveg", invert=TRUE]
    
  } else {stop("Given landtypes (currently) not supported!")}
  
  CarbonFlux2Soil <- CarbonFlux2Soil_ha * Landuse
  getNames(CarbonFlux2Soil, dim = "data") <- "lit2soilc" 
  
  #weights for !cellular needed
  
  return(list(x=CarbonFlux2Soil,
              weight=NULL,
              unit="Mt C",
              description=paste0("Carbon Input from Litter for ", landuse_types ," land use set"),
              min = 0,
              isocountries = !cellular))
}