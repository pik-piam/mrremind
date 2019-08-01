#' @title calcSeed
#' @description Calculates Seed demand
#'
#' @param cellular cellular or regional level
#' @param products kcr or also kall, which includes seeds for eggs and fish
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @param attributes in dm, wm, ge, nr, p, k
#' @return List of magpie object with results and weight on country or cellular level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("Seed")
#' }
#' @importFrom magpiesets findset

calcSeed<-function(cellular=FALSE,products="kall",irrigation=FALSE,attributes="all"){
 
  products <- findset(products,noset = "original")
  seed     <- collapseNames(calcOutput("FAOmassbalance",aggregate = FALSE)[,,"seed"][,,products])
  
  
  if(cellular==TRUE) {
    
    if(!all(products%in%findset("kcr"))){stop("cellular data only exists for kcr products")}
    
    map            <- toolMappingFile(type = "cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
    production_reg <- calcOutput("Production",products="kcr",cellular=FALSE,calibrated=TRUE,irrigation=FALSE,aggregate=FALSE)[,,products]
    seed_shr       <- collapseNames(seed[,,"dm"]/production_reg[,,"dm"])
    seed_shr[is.na(seed_shr)]       <- 0
    seed_shr[is.infinite(seed_shr)] <- 0

    production     <- calcOutput("Production",products="kcr",cellular=cellular,irrigation=irrigation,calibrated=TRUE,attributes=attributes,aggregate=FALSE)[,,products]
    seed           <- production * seed_shr[getRegions(production),,]
  } 
  
  if(any(attributes!="all")){seed <-seed[,,attributes]}

  return(list(
    x=seed,
    weight=NULL,
    unit="Mt Dm, Nr, P, K, WM or PJ Energy",
    description="Seed use",
    isocountries =!cellular))
}

