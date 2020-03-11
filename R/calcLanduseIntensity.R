#' @title calcLanduseIntensity
#' 
#' @description This function prepares total tau values for use. As the source data already
#' provides all required information this function purely removes unrequired
#' data and moves the xref values to the weighting object which is required for
#' aggregation.
#' 
#' @param rescale TRUE (default), if Tau should be rescaled in a way, that 2010 is always 1
#' @param sectoral "kcr" (default) for MAgPIE crop items and "lpj" for LPJmL crop items, "pasture" for pasture
#' @return Total tau data and corresonding weights as a list of two MAgPIE
#' objects
#' @author Benjamin Leon Bodirsky, Kristine Karstens
#' @seealso \code{\link{calcTauTotal}}, \code{\link{readTau}},
#' \code{\link{convertTau}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LanduseIntensity")
#' 
#' }
#' 
#' @importFrom madrat toolAggregate
 
calcLanduseIntensity <- function(sectoral="kcr", rescale = TRUE) {

  if(sectoral%in%c("kcr","lpj")){
    
    #Mappings
    MAGcroptypes  <- findset("kcr")
    MAGtoLPJ      <- read.csv(toolMappingFile("sectoral","MAgPIE_LPJmL.csv"))
    MAGtoLPJ      <- MAGtoLPJ[MAGtoLPJ$MAgPIE%in%MAGcroptypes,]
    LPJCroptypes  <- levels(droplevels(MAGtoLPJ$LPJmL))
    CountryToCell <- toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv = TRUE)
    
    #Load LPJ yields and area on cell level
    LPJYields      <- toolCell2isoCell(readSource("LPJmL",subtype="LPJmL5:CRU_4.harvest", convert="onlycorrect")[,,LPJCroptypes])
    if(sectoral=="kcr") LPJYields  <- toolAggregate(LPJYields, rel=MAGtoLPJ, from="LPJmL", to="MAgPIE", dim=3.1)
    LPJCroparea    <- toolCell2isoCell(calcOutput("Croparea", sectoral=sectoral, physical=TRUE, cellular=TRUE, irrigation=TRUE, aggregate = FALSE))
    
    #Getting overlapping time period
    years          <- intersect(getYears(LPJCroparea), getYears(LPJYields))
    LPJYields      <- LPJYields[,years,]
    LPJCroparea    <- LPJCroparea[,years,]

    LPJProduction  <- LPJYields * LPJCroparea
    LPJProduction  <- toolAggregate(dimSums(LPJProduction, dim=3.2), 
                                    rel=CountryToCell, from="celliso", to="iso", dim=1, partrel = TRUE)
    #Load FAO data and caluculate FAO yields on country level
    FAOProduction    <- collapseNames(calcOutput("FAOmassbalance", aggregate=FALSE)[,,"production"][,,"dm"][,,MAGcroptypes])
    
    if(sectoral == "lpj") FAOProduction    <- toolAggregate(FAOProduction, rel=MAGtoLPJ, from="MAgPIE", to="LPJmL", dim=3.1)

    #Getting overlapping time period and countries
    regions          <- intersect(getRegions(LPJProduction),getRegions(FAOProduction))
    years            <- intersect(getYears(LPJProduction),getYears(FAOProduction))
    LPJProduction    <- LPJProduction[regions, years,]
    FAOProduction    <- FAOProduction[regions, years,]
    
    #Calculate TAU as ratio of FAO to LPJmL yields
    TAU              <- FAOProduction / LPJProduction
    TAU[is.na(TAU)]  <- 0
    TAU[TAU == Inf]  <- 0
    
    CountryCroparea <- dimSums(toolAggregate(LPJCroparea, rel=CountryToCell, 
                                             from="celliso", to="iso", dim=1, partrel = TRUE), dim=3.1)
    
    #rescale such that average in 2010 is 1
    if(rescale){
      Rescale2010   <- toolNAreplace(x=TAU[,"y2010",], weight=CountryCroparea[getRegions(TAU),"y2010",])
      RescaleWeight <- dimSums(Rescale2010$x * Rescale2010$weight, dim=1) / dimSums(Rescale2010$weight,dim=1)
      TAU           <- TAU / setYears(RescaleWeight, NULL)
      TAU[is.na(TAU)]  <- 0
    }
    
    #calculate TAU aggregated over all croptypes  
    kcr2all <- matrix(c(MAGcroptypes,rep("all", length(MAGcroptypes))),ncol=2, dimnames=list(NULL, c("kcr","all")))
    TAUall  <- toolAggregate(TAU, rel=kcr2all, weight = CountryCroparea, from="kcr", to="all", dim=3)
    
    x      <- mbind(TAU,setNames(TAUall,"all"))
    weight <- CountryCroparea
    weight <- mbind(weight, setNames(dimSums(weight,dim=3.1,na.rm = TRUE),"all"))  
    out    <- toolNAreplace(x=x,weight=weight)
    x      <- toolCountryFill(out$x,fill = 0)
    weight <- toolCountryFill(out$weight,fill = 0)
 #  ?Old comment: if only one indicator is required over all crops, I suggest a weighting over area harvested
    
  } else if(sectoral=="pasture"){
    
    #Mappings
    CountryToCell <- toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv = TRUE)
    
    #Load LPJ yields and area on cell level
    LPJYields           <- toolCell2isoCell(readSource("LPJmL", subtype="LPJmL5:CRU_4.harvest", convert="onlycorrect")[,,"mgrass.rainfed"])
    MAGPasturearea      <- toolCell2isoCell(calcOutput("LanduseInitialisation", cellular = TRUE, aggregate = FALSE)[,,"past"])
    getNames(LPJYields) <- getNames(MAGPasturearea) <- "pasture"

    #Getting overlapping time period
    years          <- intersect(getYears(MAGPasturearea), getYears(LPJYields))
    LPJYields      <- LPJYields[,years,]
    MAGPastarea    <- MAGPasturearea[,years,]
 
    LPJProduction  <- LPJYields * MAGPasturearea
    LPJProduction  <- toolAggregate(LPJProduction, rel=CountryToCell, from="celliso", to="iso", dim=1, partrel = TRUE)
    
    #Load FAO data and caluculate FAO yields on country level
    FAOProduction    <- collapseNames(calcOutput("FAOmassbalance", aggregate=FALSE)[,,"production"][,,"dm"][,,"pasture"])
    
    #Getting overlapping time period and countries
    regions          <- intersect(getRegions(LPJProduction),getRegions(FAOProduction))
    years            <- intersect(getYears(LPJProduction),getYears(FAOProduction))
    LPJProduction    <- LPJProduction[regions, years,]
    FAOProduction    <- FAOProduction[regions, years,]
    
    #Calculate TAU as ratio of FAO to LPJmL yields
    TAU              <- FAOProduction/ LPJProduction
    TAU[is.na(TAU)]  <- 0
    TAU[TAU == Inf]  <- 0
    
    CountryPastarea       <- toolAggregate(MAGPastarea, rel=CountryToCell, from="celliso", to="iso", dim=1, partrel = TRUE)
    
    #rescale such that average in 2010 is 1
    if(rescale){
      Rescale2010   <- toolNAreplace(x=TAU[,"y2010",], weight=CountryPastarea[getRegions(TAU),"y2010",])
      RescaleWeight <- dimSums(Rescale2010$x * Rescale2010$weight, dim=1) / dimSums(Rescale2010$weight,dim=1)
      TAU           <- TAU / setYears(RescaleWeight, NULL)
    }

    x      <- TAU
    weight <- CountryPastarea[getRegions(TAU),getYears(TAU),]
    out    <- toolNAreplace(x=x,weight=weight)
    x      <- toolCountryFill(out$x,fill = 0)
    weight <- toolCountryFill(out$weight,fill = 0)
    
    
  } else {stop("Not possible (for now) for the given item set (sectoral)!")}
  
  return(list(x=x,
              weight=weight,
              unit="",
              description="FAO production devided by LPJml yield times LUH areas for MAgPIE representative crops and pasture",
              note=c("")))
}
