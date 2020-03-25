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
#' @importFrom magclass getSets magpie_expand


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
      
      LPJYields      <- toolCell2isoCell(readSource("LPJmL",subtype="LPJmL5:CRU_4.harvest", convert="onlycorrect")[,selectyears,])
      
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
      
      MAGProduction  <- MAGYields * MAGCroparea
      
      #####################################################################  
      # correct production mismatch - generic approach
      
      isoMAGProduction  <- isoMismatch <- toolAggregate(dimSums(MAGProduction, dim=3.2), rel = CountryToCell, from = "celliso", to = "iso")
      FAOProduction     <- collapseNames(calcOutput("FAOmassbalance", aggregate=FALSE)[,selectyears,"production"][,,getNames(MAGProduction, dim=1)][,,"dm"])
      isoMismatch[]     <- abs(round(isoMAGProduction-toolIso2CellCountries(FAOProduction),4)) > 0
      
      if(any(isoMismatch!=0)){
        
        # correct items with no area
        isoMAGCroparea   <- noMAGCroparea <- toolAggregate(MAGCroparea, rel = CountryToCell, from = "celliso", to = "iso")
        noMAGCroparea[]  <- (isoMAGCroparea==0)*isoMismatch
        
        if(any(noMAGCroparea!=0)){
          
          # distribute equally over all non-irrigated cells first
          MAGProduction[,,"rainfed"]    <- MAGProduction[,,"rainfed"]*(1-noMAGCroparea[,,"rainfed"]) + 
            magpie_expand(noMAGCroparea[,,"rainfed"] * (toolIso2CellCountries(FAOProduction)-isoMAGProduction), MAGProduction[,,"rainfed"])/
            new.magpie(names(getCPR(MAGProduction)),fill=getCPR(MAGProduction))
          
          isoMAGProduction  <- toolAggregate(dimSums(MAGProduction, dim=3.2), rel = CountryToCell, from = "celliso", to = "iso")
          
          
          MAGProduction[,,"irrigated"]   <- MAGProduction[,,"irrigated"]*(1-noMAGCroparea[,,"irrigated"]) + 
            magpie_expand(noMAGCroparea[,,"irrigated"] * (toolIso2CellCountries(FAOProduction)-isoMAGProduction), MAGProduction[,,"irrigated"])/
            new.magpie(names(getCPR(MAGProduction)),fill=getCPR(MAGProduction))
        }
        
        # correct items with no yields
        isoMAGYields   <- noMAGYields <- toolAggregate(MAGYields, weight=MAGCroparea, rel = CountryToCell, from = "celliso", to = "iso")
        noMAGYields[]  <- (isoMAGYields==0)*isoMismatch*(1-noMAGCroparea)
        
        if(any(noMAGYields!=0)){
          
          isoMAGProduction  <- toolAggregate(dimSums(MAGProduction, dim=3.2), rel = CountryToCell, from = "celliso", to = "iso")
          
          # distribute corresponding to crop area share
          MAGProduction[,,"rainfed"]   <- MAGProduction[,,"rainfed"]*(1-noMAGYields[,,"rainfed"]) +
            noMAGYields[,,"rainfed"]*toolAggregate(toolIso2CellCountries(FAOProduction)-isoMAGProduction, rel= CountryToCell, weight=MAGCroparea[,,"rainfed"], from="iso", to="celliso")
          
          
          isoMAGProduction  <- toolAggregate(dimSums(MAGProduction, dim=3.2), rel = CountryToCell, from = "celliso", to = "iso")
          
          # distribute corresponding to crop area share
          MAGProduction[,,"irrigated"]   <- MAGProduction[,,"irrigated"]*(1-noMAGYields[,,"irrigated"]) +
            noMAGYields[,,"irrigated"]*toolAggregate(toolIso2CellCountries(FAOProduction)-isoMAGProduction, rel= CountryToCell, weight=MAGCroparea[,,"irrigated"], from="iso", to="celliso")
        }
        
      }
      
      isoMAGProduction  <- isoMismatch <- toolAggregate(dimSums(MAGProduction, dim=3.2), rel = CountryToCell, from = "celliso", to = "iso")
      isoMismatch[]     <- abs(round(isoMAGProduction-toolIso2CellCountries(FAOProduction),4)) > 0
      if(any(isoMismatch!=0)) warning("Cellular data to FAO production mismatch after generic fix. Please check.")
      #####################################################################
      
      
      if(!irrigation){ MAGProduction  <- dimSums(MAGProduction, dim=3.2)}
      
      ProdAttributes <- calcOutput("Attributes", aggregate = FALSE)[,,MAGcroptypes]
      
      if(any(attributes!="all")){
        ProdAttributes<-ProdAttributes[,,attributes]
      }
      
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
      
      PastureArea    <- toolCell2isoCell(collapseNames(calcOutput("LanduseInitialisation", cellular=TRUE, aggregate = FALSE)[,selectyears,"past"]))
      PastureYields  <- toolCell2isoCell(collapseNames(readSource("LPJmL", subtype="LPJmL5:CRU_4.harvest", convert="onlycorrect")[,selectyears,"mgrass"][,,"rainfed"]))
      CountryToCell  <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
      
      if(calibrated==TRUE){
        
        TAU            <- calcOutput("LanduseIntensity", sectoral = "pasture", rescale = FALSE, aggregate = FALSE)[,selectyears,]
        TAUCell        <- toolAggregate(x = TAU, rel = CountryToCell, from = "iso", to = "celliso", partrel = TRUE)
        
        PastureYields  <- TAUCell * PastureYields
      }
      
      MAGProduction    <- PastureYields * PastureArea
      
      #####################################################################  
      # correct production mismatch - generic approach
      
      isoMAGProduction  <- isoMismatch <- toolAggregate(MAGProduction, rel = CountryToCell, from = "celliso", to = "iso")
      FAOProduction     <- collapseNames(calcOutput("FAOmassbalance", aggregate=FALSE)[,selectyears,"production"][,,"pasture"][,,"dm"])
      isoMismatch[]     <- abs(round(isoMAGProduction-toolIso2CellCountries(FAOProduction),4)) > 0
      
      if(any(isoMismatch!=0)){
        
        # correct items with no area
        isoPastureArea   <- noPastureArea <- toolAggregate(PastureArea, rel = CountryToCell, from = "celliso", to = "iso")
        noPastureArea[]  <- (isoPastureArea==0)*isoMismatch
  
        if(any(noPastureArea!=0)){
          
          # distribute equally over all cells
          MAGProduction     <- MAGProduction*(1-noPastureArea) + 
                               magpie_expand(noPastureArea * toolIso2CellCountries(FAOProduction), MAGProduction)/
                                 new.magpie(names(getCPR(MAGProduction)),fill=getCPR(MAGProduction))
          
        }
        
        # correct items with no yields
        isoPastureYields   <- noPastureYields <- toolAggregate(PastureYields, weight=PastureArea, rel = CountryToCell, from = "celliso", to = "iso")
        noPastureYields[]  <- (isoPastureYields==0)*isoMismatch*(1-noPastureArea)
        
        if(any(noPastureYields!=0)){
          
          # distribute corresponding to pasture area share
          MAGProduction     <- MAGProduction*(1-noPastureYields) +
                               noPastureYields*toolAggregate(toolIso2CellCountries(FAOProduction), rel= CountryToCell, weight=PastureArea, from="iso", to="celliso")
        }
      }
      
      isoMAGProduction  <- isoMismatch <- toolAggregate(MAGProduction, rel = CountryToCell, from = "celliso", to = "iso")
      isoMismatch[]     <- abs(round(isoMAGProduction-toolIso2CellCountries(FAOProduction),4)) > 0
      if(any(isoMismatch!=0)) warning("Cellular data to FAO production mismatch after generic fix. Please check.")
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
