#' @title calcCarbonResidues
#' @description Calculates carbon input from residues for cropland soils (other land use types set to 0).
#'
#' @param landuse_types Sets: carbon or ini_new (magpie initialisation)
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' 
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CarbonResidues")
#' }
#' @importFrom magclass dimCode

calcCarbonResidues <- function(landuse_types="carbon", cellular = TRUE){
  
  ResidueRecyclingAg  <- collapseNames(calcOutput("ResFieldBalancePast", cellular=TRUE, aggregate = FALSE)[,,"recycle"][,,"c"])
  ResidueRecyclingBg  <- dimSums(calcOutput("ResBiomass", cellular=TRUE, aggregate = FALSE)[,,"bg"][,,"c"], dim=3)
  ResidueRecycling    <- ResidueRecyclingBg + ResidueRecyclingAg 
  
  UptakeRateResidue   <- 0.3
  
  CarbonFlux2Soil     <- ResidueRecycling * UptakeRateResidue
  
  if(!cellular){
    
    CountryToCell     <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
    CarbonFlux2Soil   <- toolAggregate(CarbonFlux2Soil, rel=CountryToCell, from="cell", to="iso", partrel=TRUE)
    
  }
  
  CarbonFlux2Soil <- add_dimension(CarbonFlux2Soil, dim = 3.1, add = "landuse", nm = "crop")
  getNames(CarbonFlux2Soil, dim=2) <- "res2soilc"
  
  ##################
  ### total set  ###
  ##################
  if(landuse_types=="carbon"){
    
    complete_residues <- c("urban","past", "natveg")
    CarbonFlux2Soil   <- add_columns(CarbonFlux2Soil, addnm=complete_residues, dim = dimCode("landuse", CarbonFlux2Soil))
    CarbonFlux2Soil[,,complete_residues] <- 0 
    
  } else if(landuse_types=="ini_new"){
    
    complete_residues <- c("urban","past", "natveg")
    CarbonFlux2Soil   <- add_columns(CarbonFlux2Soil, addnm=complete_residues, dim = dimCode("landuse", CarbonFlux2Soil))
    CarbonFlux2Soil[,,complete_residues] <- 0
    
    natveg_disagg    <- c("primforest","secdforest","forestry","other")
    CarbonFlux2Soil  <- add_columns(CarbonFlux2Soil, addnm=natveg_disagg, dim = dimCode("landuse", CarbonFlux2Soil))
    CarbonFlux2Soil[,,natveg_disagg] <- CarbonFlux2Soil[,,"natveg"]
    CarbonFlux2Soil  <- CarbonFlux2Soil[,,"natveg", invert=TRUE]
    
  } else {stop("Given landtypes (currently) not supported!")}
  
  
  return(list(x=CarbonFlux2Soil,
              weight=NULL,
              unit="Mt C",
              description=paste0("Carbon Input from Residues for ", landuse_types ," land use set"),
              min = 0,
              isocountries =!cellular))
}