#' Convert FRA 2015 data
#' Update dd-Jmm-jjjj - Please add comment if changes made here (Abhi)
#' 
#' @param x MAgPIE object containing original values
#' @param subtype The FAO FRA 2015 file type, e.g.: fac, production, biodiversity or anndat.
#' @return Data as MAgPIE object with common country list
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}},
#' @examples
#' 
#' \dontrun{ a <- readSource("FRA2015","production",convert=TRUE)}
#' @importFrom magclass magpiesort
#' 

convertFAO_FRA2015 <- function(x,subtype){
  if(any(c("fac","production","biodiversity","anndat")%in% subtype)){
    x <- toolCountryFill(x,fill = 0)
    if (any(c("ProdFor","MulUseFor") %in% getNames(x))) {
      x[,,"ProdFor"]    <- x[,,"ProdFor"]  /1000 # conversion from 1000ha to Million ha 
      x[,,"MulUseFor"]  <- x[,,"MulUseFor"]/1000 # conversion from 1000ha to Million ha 
    }
    if (any(c("NetAnnIncr","IncrConif", "IncrBroa")%in% getNames(x))) {
      x[,,"NetAnnIncr"] <- x[,,"NetAnnIncr"]*1000 # conversion from m3/ha/yr to mil.m3/mil.ha/yr 
      x[,,"IncrConif"]  <- x[,,"IncrConif"]  *1000 # conversion from m3/ha/yr to mil.m3/mil.ha/yr
      x[,,"IncrBroa"]   <- x[,,"IncrBroa"]   *1000 # conversion from m3/ha/yr to mil.m3/mil.ha/yr
    }
    if (any(c("Forest","Forchange","NatFor","Nfchange","OthWooLan","OthLan","LanTreCov","InWater","Landarea","PrimFor","NatRegFor","IntroSpp","NatzedSpp","PlantFor","Pfchange","IntroSppPlant"
)%in% getNames(x))){
      NamesToFix <- c("Forest","Forchange","NatFor","Nfchange","OthWooLan","OthLan","LanTreCov","InWater","Landarea","PrimFor","NatRegFor","IntroSpp","NatzedSpp","PlantFor","Pfchange","IntroSppPlant","Mangrove")
      for(i in NamesToFix){
        x[,,i] <- x[,,i]/1000 #conversion from 1000ha to Million ha
      }
    }
    if(any(c("ForExp","Afforest","NatForExp","Deforest","HumDef","Reforest","ArtRef")%in%getNames(x))){
      NamesToFix <- c("ForExp","Afforest","NatForExp","Deforest","HumDef","Reforest","ArtRef")
      for(i in NamesToFix){
        x[,,i] <- x[,,i]/1000000 #conversion from ha/yr to Mil.ha/yr
      }
    }
    if (any(c("BioCons","ProtArea") %in% getNames(x))){
      x[,,"BioCons"]   <- x[,,"BioCons"]   /1000 # conversion from 1000ha to Million ha
      x[,,"ProtArea"]  <- x[,,"ProtArea"]  /1000 # conversion from 1000ha to Million ha
    }
    if (any(c("WooRemov","WooFuel","WooRW") %in% getNames(x))){
      x[,,"WooRemov"]  <- x[,,"WooRemov"]  /1000 # conversion from 1000m3 to Million m3
      x[,,"WooFuel"]   <- x[,,"WooFuel"]   /1000 # conversion from 1000m3 to Million m3
      x[,,"WooRW"]     <- x[,,"WooRW"]     /1000 # conversion from 1000m3 to Million m3
    }
    return(x)
  } else {stop("Invalid subtype ", subtype)}
}