#' @title calcCarbonSoilIni
#' @description Initialize soil carbon pools for land use types.
#'
#' @param landuse_types Sets: carbon or ini_new (magpie initialisation)
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' 
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CarbonSoilIni")
#' }
#' @importFrom magclass dimCode

calcCarbonSoilIni <- function(landuse_types="carbon", cellular=TRUE){
  
  if(!cellular)(stop("For the moment just on cellular level"))
  
  Landuse           <- calcOutput("Landuse",              landuse_types="carbon", cellular=cellular, aggregate=FALSE)
  Landuse           <- mbind(calcOutput("Landuse",        landuse_types="carbon", cellular=cellular, selectyears="y1960", aggregate=FALSE), Landuse)
  
  ##################
  ### single     ###
  ##################
  
  LPJml2MAgPIE  <- c(past = "lu_grass", natveg = "nat_veg", crop = "lu_maize_nres_rf") 
  Soilc_ha      <- mbind(calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["crop"]), subtype="soilc", aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["past"]), subtype="soilc",  aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["natveg"]), subtype="soilc", aggregate=FALSE))
  AddFirstYears <- mbind(calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["crop"]), subtype="soilc", selectyears="y1960", aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["past"]), subtype="soilc", selectyears="y1960", aggregate=FALSE),
                          calcOutput("LPJmlCarbon", landtype=unname(LPJml2MAgPIE["natveg"]), subtype="soilc", selectyears="y1960", aggregate=FALSE))
  Soilc_ha      <- mbind(AddFirstYears, Soilc_ha)
  
  ##################
  ### total set  ###
  ##################
  
  if(landuse_types=="carbon"){
    
    Soilc_ha            <- add_columns(Soilc_ha, addnm="urban", dim = dimCode("landuse", Soilc_ha))
    Soilc_ha[,,"urban"] <- 0.80 * Soilc_ha[,,"natveg"]
      
  } else if(landuse_types=="ini_new"){
    
    Soilc_ha            <- add_columns(Soilc_ha, addnm="urban", dim = dimCode("landuse", Soilc_ha))
    Soilc_ha[,,"urban"] <- 0.80 * Soilc_ha[,,"natveg"]
    
    natveg_disagg <- c("primforest","secdforest","forestry","other")
    Soilc_ha      <- add_columns(Soilc_ha, addnm=natveg_disagg, dim = dimCode("landuse", Soilc_ha))
    Soilc_ha[,,natveg_disagg] <- Soilc_ha[,,"natveg"]
    Soilc_ha      <- Soilc_ha[,,"natveg", invert=TRUE]
    
  } else {stop("Given landtypes (currently) not supported!")}

  SoilCarbon <- Soilc_ha * Landuse
  SoilCarbon[which(is.na(SoilCarbon))] <- 0
  
  return(list(x=SoilCarbon,
              weight=Landuse,
              unit="Mt C",
              description=paste0("Soil carbon for ", landuse_types ," land use set"),
              min = 0,
              isocountries = !cellular))
}