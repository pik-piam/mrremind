#' @title calcProduction
#' @description Distributes crop, pasture and livestock production in space to 0.5 degree
#'
#' @param products setname of products ("kcr", "kli", "pasture")
#' @param cellular if TRUE production is calculate on cellular level
#' @param calibrated if FALSE, lpj yields will be used uncalibrated, if true, calibrated on FAP production on country level
#' @param attributes "All" for all crop attributes, or specify e.g. DM (dry matter), Nr (nitrogen) for memory reduction
#' @param irrigation if TRUE, additional information on irrigated and rainfed production is provided
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcLanduseInitialisation}},
#' \code{\link{calcCroparea}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("Production")
#' }
#' 
#' @importFrom magclass getSets


calcProduction<-function(products="kcr", cellular=FALSE, calibrated=TRUE, attributes="all", irrigation=FALSE){
  
  selectyears <- findset("past")
  
  if (products=="kcr"){
    
    MAGcroptypes  <- findset("kcr")
    missing <- c("betr","begr")
    MAGcroptypes  <- setdiff(MAGcroptypes, missing)
    
    if(!cellular){
      if(irrigation){stop("Irrigation not yet implemented for this resolution")}
      MAGProduction <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,,"production"][,selectyears,MAGcroptypes])
      MAGProduction <- add_columns(MAGProduction,addnm = missing,dim = 3.1)
      MAGProduction[,,missing]<-0
      
    } else {
      
      #################################
      ### crop production celluluar ###
      #################################
      
      LPJYields      <- toolCell2isoCell(readSource("LPJml_rev21",subtype="harvest_lai4", convert=FALSE)[,selectyears,])
      
      CountryToCell  <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
      MAGtoLPJ       <- toolMappingFile(type="sectoral",name = "MAgPIE_LPJmL.csv",readcsv = TRUE)
      MAGtoLPJ       <- MAGtoLPJ[which(MAGtoLPJ$MAgPIE %in% MAGcroptypes),]
      
      MAGYields      <- toolAggregate(x = LPJYields, rel = MAGtoLPJ, from = "LPJmL", to="MAgPIE", dim = 3.1, partrel = TRUE)
      MAGCroparea    <- toolCell2isoCell(calcOutput("Croparea", sectoral="kcr", physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate = FALSE)[,selectyears,MAGcroptypes])  
      
      if(calibrated==TRUE){
        
        TAU            <- calcOutput("LanduseIntensity", sectoral = "kcr", rescale = FALSE, aggregate = FALSE)[,selectyears,MAGcroptypes]
        TAUCell        <- toolAggregate(x = TAU, rel = CountryToCell, from = "iso", to = "celliso", partrel = TRUE)
        
        MAGYields      <- TAUCell * MAGYields 
      }
      
      ProdAttributes <- calcOutput("Attributes", aggregate = FALSE)[,,MAGcroptypes]
      
      if(any(attributes!="all")){
        ProdAttributes<-ProdAttributes[,,attributes]
      }
      
      MAGProduction  <- MAGYields * MAGCroparea
      if(!irrigation){MAGProduction  <- dimSums(MAGProduction, dim=3.2)}
      
      MAGProduction  <- MAGProduction * ProdAttributes
      MAGProduction  <-add_columns(MAGProduction,addnm = missing,dim = 3.1)
      MAGProduction[,,missing]<-0
    }
    
  } else if (products=="pasture"){
    if(irrigation){stop("Irrigation not yet implemented for this Product group")}
    if(!cellular){
      
      MAGProduction  <- collapseNames(calcOutput("FAOmassbalance", aggregate=FALSE)[,,"production"][,selectyears,"pasture"])
      
    } else {
      
      ####################################
      ### pasture production celluluar ###
      ####################################
      
      Pasturearea    <- toolCell2isoCell(collapseNames(calcOutput("LanduseInitialisation", cellular=TRUE, aggregate = FALSE)[,selectyears,"past"]))
      PastureYields  <- toolCell2isoCell(collapseNames(readSource("LPJml_rev21", subtype="harvest_lai4", convert=FALSE)[,selectyears,"mgrass"][,,"rainfed"]))
      CountryToCell  <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
      
      if(calibrated==TRUE){
      
        TAU            <- calcOutput("LanduseIntensity", sectoral = "pasture", rescale = FALSE, aggregate = FALSE)[,selectyears,]
        TAUCell        <- toolAggregate(x = TAU, rel = CountryToCell, from = "iso", to = "celliso", partrel = TRUE)
        
        PastureYields  <- TAUCell * PastureYields
      }
    
      MAGProduction    <- PastureYields * Pasturearea
      
      #####################################################################  
      #correct strange pasture production for DJI, EGY, SAU YEM, KWT and AWE
      
      WrongRegions      <- c("EGY","SAU","YEM","KWT","ARE","DJI")
      FAOProduction     <- collapseNames(calcOutput("FAOmassbalance", aggregate=FALSE)[WrongRegions,selectyears,"production"][,,"pasture"][,,"dm"])
      
      #correct strange data for 2005 and 2010
      WrongRegions05    <- c("EGY","SAU","YEM","KWT","ARE")
      WrongYears        <- c("y2005","y2010")
      
      PastureYields[WrongRegions05,"y2005",] <- setYears(PastureYields[WrongRegions05,"y2000",],NULL)
      PastureYields[WrongRegions05,"y2010",] <- setYears(PastureYields[WrongRegions05,"y2000",],NULL)
      
      DummyProduction   <- PastureYields[WrongRegions05,WrongYears,] * Pasturearea[WrongRegions05,WrongYears,]
      DummyProductionCountry <- toolAggregate(DummyProduction, rel = CountryToCell, from = "celliso", to = "iso", partrel = TRUE)
      
      Calibration       <- FAOProduction[WrongRegions05,WrongYears,] / DummyProductionCountry
      Wrong05Production <- DummyProduction * Calibration
      
      MAGProduction[WrongRegions05,WrongYears,] <- Wrong05Production
      
      #correct strange data for past
      WrongRegionsEver     <- "DJI"
      PastureShare         <- Pasturearea[WrongRegionsEver,,] / dimSums(Pasturearea[WrongRegionsEver,,], dim=1)
      WrongEverProduction  <- PastureShare * FAOProduction[WrongRegionsEver,,]
      
      MAGProduction[WrongRegionsEver,,] <- WrongEverProduction
      
      #####################################################################  
    
      ProdAttributes <- calcOutput("Attributes", aggregate = FALSE)[,,"pasture"]
      MAGProduction  <- collapseNames(MAGProduction * ProdAttributes)
      
    }
      
  } else if (products=="kli"){
    
    Livestocktypes <- findset("kli")
    if(irrigation){stop("Irrigation not yet implemented for this Product group")}
    if(!cellular){

      MAGProduction <- collapseNames(calcOutput("FAOmassbalance_pre", aggregate=FALSE)[,selectyears,Livestocktypes][,,"production"])
      
    } else {
    
      MAGProduction       <- calcOutput("LivestockGridded", aggregate = FALSE)
    }
    
  } else {stop("Products so far can only be kcr,kli,or pasture")}
  
  
  x      <- MAGProduction
  
  if(any(attributes!="all")){
    x <-x[,,attributes]
  }
  
  return(list(x=x,
              weight=NULL,
              unit="Mt DM/Nr/P/K/WM or PJ energy",
              description="Crop, pasture and livestock production: dry matter: Mt (dm), gross energy: PJ (ge), reactive nitrogen: Mt (nr), phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm).",
              min=-Inf,
              max=Inf, 
              isocountries=!cellular
              )
         
  ) 
}

