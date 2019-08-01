#' @title calcCarbonManure
#' @description Calculates carbon input from manure for cropland and pasture soils (other land use types set to 0).
#'
#' @param landuse_types Sets: carbon or ini_new (magpie initialisation)
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' 
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CarbonManure")
#' }
#' @importFrom magclass dimCode

calcCarbonManure <- function(landuse_types="carbon", cellular=TRUE){
  
  if(!cellular)(stop("For the moment just on cellular level"))
  
  CNratioManure     <- 13
  UptakeRateManure  <- 0.3
  

  ManureApplication <- collapseNames(calcOutput("ManureRecyclingCroplandPast", cellular = cellular, aggregate = FALSE)[,,"nr"])
  ManureGrazing     <- collapseNames(dimSums(calcOutput("Excretion", cellular = cellular, aggregate = FALSE)[,,"stubble_grazing"][,,"nr"], dim=3.2))
  ManureInput       <- (ManureApplication + ManureGrazing) * CNratioManure
  ManureInput       <- add_dimension(ManureInput, dim = 3.1, add = "landuse", nm = "crop")
  getNames(ManureInput, dim=2) <- "man2soilc"
  
  ManureInput       <- add_columns(ManureInput, addnm = "past", dim = dimCode("landuse", ManureInput))
  ManureGrazing     <- collapseNames(dimSums(calcOutput("Excretion", cellular = cellular, aggregate = FALSE)[,,"grazing"][,,"nr"], dim=3.2))
  ManureInput[,,"past"] <- ManureGrazing * CNratioManure

  CarbonFlux2Soil   <- ManureInput * UptakeRateManure
  
  if(!cellular){
    
    CountryToCell     <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
    CarbonFlux2Soil   <- toolAggregate(CarbonFlux2Soil, rel=CountryToCell, from="cell", to="iso", partrel=TRUE)
    
  }
 
  
  ##################
  ### total set  ###
  ##################
  if(landuse_types=="carbon"){
    
    complete_manure   <- c("urban", "natveg")
    CarbonFlux2Soil   <- add_columns(CarbonFlux2Soil, addnm=complete_manure, dim = dimCode("landuse", CarbonFlux2Soil))
    CarbonFlux2Soil[,,complete_manure] <- 0 
    
  } else if(landuse_types=="ini_new"){
    
    complete_manure   <- c("urban", "natveg")
    CarbonFlux2Soil   <- add_columns(CarbonFlux2Soil, addnm=complete_manure, dim = dimCode("landuse", CarbonFlux2Soil))
    CarbonFlux2Soil[,,complete_manure] <- 0 
    
    natveg_disagg    <- c("primforest","secdforest","forestry","other")
    CarbonFlux2Soil  <- add_columns(CarbonFlux2Soil, addnm=natveg_disagg, dim = dimCode("landuse", CarbonFlux2Soil))
    CarbonFlux2Soil[,,natveg_disagg] <- CarbonFlux2Soil[,,"natveg"]
    CarbonFlux2Soil  <- CarbonFlux2Soil[,,"natveg", invert=TRUE]
    
  } else {stop("Given landtypes (currently) not supported!")}
  
  
  return(list(x=CarbonFlux2Soil,
              weight=NULL,
              unit="Mt C",
              description=paste0("Carbon Input from Manure for ", landuse_types ," land use set"),
              min = 0,
              isocountries =!cellular))
}