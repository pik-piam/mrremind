#' @title calcCarbonBudget
#' @description Calculates Carbon Budgets for Cropland soils on country levels.
#'
#' @param decomp_fix default FALSE (if TRUE )
#' @param cellular if TRUE value is calculate and returned (set aggregate to FALSE!) on cellular level
#' 
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CarbonBudget")
#' }
#' @importFrom magclass setNames

calcCarbonBudget <- function(decomp_fix=FALSE, cellular=FALSE){
  
  if(!cellular)(stop("For the moment just on cellular level"))
  
  ######################
  ### Load Data      ###
  ######################
  
  InitialSoilCarbon <- calcOutput("CarbonSoilIni",        landuse_types="carbon", cellular=cellular, aggregate=FALSE)
  Landuse           <- calcOutput("Landuse",              landuse_types="carbon", cellular=cellular, aggregate=FALSE)
  Landuse           <- mbind(calcOutput("Landuse",        landuse_types="carbon", cellular=cellular, selectyears="y1960", aggregate=FALSE), Landuse)
  
  LanduseChange     <- calcOutput("LanduseChange",        landuse_types="carbon", cellular=cellular, aggregate=FALSE)
  
  Litter            <- calcOutput("CarbonLitter",         landuse_types="carbon", cellular = cellular, aggregate = FALSE)
  Residues          <- calcOutput("CarbonResidues",       landuse_types="carbon", cellular = cellular, aggregate = FALSE)
  Manure            <- calcOutput("CarbonManure",         landuse_types="carbon", cellular = cellular, aggregate = FALSE)
  CarbonInput       <- dimSums(mbind(collapseNames(Litter, collapsedim = "climate"), Residues, Manure), dim="data")
  #quickfix
  CarbonInput[which(CarbonInput<0)] <- 0
  CarbonInput[which(CarbonInput>4.14)] <- 4.14
  
  
  if(!decomp_fix){
    
    DecompositionRate <- calcOutput("CarbonMineralization", landuse_types="carbon", cellular=cellular, aggregate=FALSE)
    #quickfix
    DecompositionRate[DecompositionRate>2] <- 2
    
  } else {
    
    # Using Litter input as reverted decomposition rate
    Litter[,,"crop"]  <- 3/7 * calcOutput("LPJmlCarbon", landtype="lu_maize_nres_rf", subtype="mrh_litter", aggregate = FALSE) * Landuse[,findset("past"),"crop"]
    Litter[,,"urban"] <- 0
    
    DecompositionRate <- collapseNames(Litter/InitialSoilCarbon[,findset("past"),], collapsedim = "data")
    getNames(DecompositionRate, dim="data") <- "decomp_rate"
    DecompositionRate <- toolNAreplace(DecompositionRate)$x
  }
  
  
  ######################
  ### Looping        ###
  ######################
  
  SoilCarbon <- SoilCarbonDensity <- InitialSoilCarbon
  years      <- as.integer(substring(findset("past"),2))
  
  for(year_x in years){
    
    SoilCarbon[,year_x,] <- setYears(SoilCarbon[,(year_x-5),],nm=year_x)
    
    SoilCarbonDensity[,year_x,] <- toolNAreplace(setYears(SoilCarbon[,year_x-5,] / Landuse[,year_x-5,],nm=year_x))$x
    CarbonFrom    <- collapseNames(SoilCarbonDensity[,year_x,] * LanduseChange[,year_x,"reduction"] ) 
    CarbonTo      <- collapseNames(dimSums(CarbonFrom, dim=3) * LanduseChange[,year_x,"expansion"] / (dimSums(LanduseChange[,year_x,"expansion"], dim=3) + 1e-16))
    

    for (timesteps in 1:5){
    
      ################
      ### Transfer ###
      ################
      
      SoilCarbon[,year_x,]    <- SoilCarbon[,year_x,] + CarbonTo/5 - CarbonFrom/5
      
      ######################
      ### Mineralization ###
      ######################
      
      SoilCarbon[,year_x,] <- SoilCarbon[,year_x,] * (1 - DecompositionRate[,year_x,])  
      
      ################
      ### Inputs   ###
      ################
      
      SoilCarbon[,year_x,] <- SoilCarbon[,year_x,] + CarbonInput[,year_x,]
      
      
      
      print(timesteps)
    }
    print(year_x)
  }
  
 
  out <- SoilCarbon

  return(list( x            = out,
               weight       = NULL,
               unit         = "Mt C",
               description  = "Carbon budget on croplands for historical period",
               isocountries = !cellular)
    )
}

