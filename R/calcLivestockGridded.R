#' @title calcLivestockGridded
#' @description Distributes crop, pasture and livestock production in space to 0.5 degree
#'
#' @return List of magpie objects with results on cellular level, weights on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("calcLivestockGridded")
#' }
#' 

calcLivestockGridded <- function(){
  
  selectyears <- findset("past")
  
  CountryToCell       <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
  LivestockProduction <- calcOutput("FAOmassbalance", aggregate=FALSE)[,selectyears,"production"]
  
  ### Ruminents
  
  Ruminents             <- c("livst_milk", "livst_rum")
  
  #Divide ruminents in extensive and intensive depending on feedmix
  RuminantFeed          <- collapseNames(calcOutput("FeedPast",nutrients="nr",aggregate = FALSE)[,selectyears,c("alias_livst_milk","alias_livst_rum")])
  CropbasedFeed         <- dimSums(RuminantFeed[,,"pasture",invert=TRUE], dim=3.2) / dimSums(RuminantFeed, dim=3.2)
  PastbasedFeed         <- collapseNames(RuminantFeed[,,"pasture"]) / dimSums(RuminantFeed, dim=3.2)
  CropbasedFeed[is.na(CropbasedFeed)]   <- 0
  PastbasedFeed[is.na(PastbasedFeed)]   <- 0
  
  getNames(PastbasedFeed) <- getNames(CropbasedFeed) <- Ruminents    
  
  RuminentProduction    <- collapseNames(LivestockProduction[,,Ruminents])
  ExtensiveRuminent     <- RuminentProduction * CropbasedFeed
  IntensiveRuminent     <- RuminentProduction * PastbasedFeed
  
  #calculate extensive ruminent production per cell from pasture production share
  PastureProduction     <- collapseNames(calcOutput("Production", products="pasture", cellular=TRUE, calibrated=TRUE, aggregate=FALSE)[,selectyears,"nr"])
  ExtensiveRuminentCell <- toolAggregate(toolIso2CellCountries(ExtensiveRuminent), rel=CountryToCell, weight=PastureProduction, from="iso", to="celliso", dim=1) 

  #calculate intensive ruminent production per cell from cropland share
  CropProduction        <- dimSums(collapseNames(toolCell2isoCell(calcOutput("Production", products="kcr", cellular=TRUE, aggregate=FALSE))[,,"dm"][,,c("betr","begr"),invert=TRUE]),dim=3)
  IntensiveRuminentCell <- toolAggregate(toolIso2CellCountries(IntensiveRuminent), rel=CountryToCell, weight=CropProduction, from="iso", to="celliso", dim=1)

  RuminantProdCell      <- ExtensiveRuminentCell + IntensiveRuminentCell
  
  ### Poultry & Pig
  
  Poultry               <- c("livst_chick", "livst_egg")
  Pig                   <- "livst_pig"
  
  #Divide pigs and poultry in extensive and intensive depending on development state
  
  DevelopmentState      <- setNames(collapseNames(calcOutput("DevelopmentState",aggregate = FALSE)[,selectyears,"SSP2"]),nm=Poultry[1])       
  DevelopmentState      <- mbind(DevelopmentState, setNames(DevelopmentState, nm = Poultry[2]))
  DevelopmentState      <- mbind(DevelopmentState, setNames(collapseNames(calcOutput("DevelopmentState", upper= 30000, aggregate = FALSE)[,selectyears,"SSP2"]), nm=Pig))
  
  PigPoultryProduction  <- collapseNames(LivestockProduction[,,c(Pig, Poultry)])
  ExtensivePigPoultry   <- PigPoultryProduction * (1 - DevelopmentState)
  IntensivePigPoultry   <- PigPoultryProduction * DevelopmentState
  
  #calculate extensive poultry and pig production per cell from urban area share
  #vcat(2,"Even better use population than urban area")
  Urbanarea               <- toolCell2isoCell(calcOutput("LanduseInitialisation", cellular=TRUE, aggregate = FALSE)[,selectyears,"urban"])
  ExtensivePigPoultryCell <- toolAggregate(toolIso2CellCountries(ExtensivePigPoultry), rel=CountryToCell, weight=Urbanarea, from="iso", to="celliso", dim=1) 
  
  #calculate intensive pig poultry production per cell from cropland share
  #more ideas to come for pig poultry disaggregation
  IntensivePigPoultryCell <- toolAggregate(toolIso2CellCountries(IntensivePigPoultry), rel=CountryToCell, weight=CropProduction, from="iso", to="celliso", dim=1)
  
  PigPoultryProdCell      <- ExtensivePigPoultryCell + IntensivePigPoultryCell
  
  ### Total Livestock
  MAGProduction           <- mbind(RuminantProdCell, PigPoultryProdCell)
  
  return(list(x=MAGProduction,
              weight=NULL,
              unit="Mt DM/Nr/P/K/WM or PJ energy",
              description="Cellular livestock production: dry matter: Mt (dm), gross energy: PJ (ge), reactive nitrogen: Mt (nr), phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm).",
              min=-Inf,
              max=Inf, 
              isocountries=FALSE)
  ) 
}