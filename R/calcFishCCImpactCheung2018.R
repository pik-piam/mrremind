#' @title calcFishCCImpactCheung2018
#' @description derive compatible Cheung data for magpie objects x_capture 
#' @return Cheung data for each FAO Major Fishing area 
#' @param subtype "ModelOutputDBEM" DBEM Model output for RCP2.6;RCP8.5
#' "ModelOutputDynModel" Dynamic Model output for RCP2.6;RCP8.5
#' @author Benjamin Bodirsky, Jasmin Wehner
#' @examples
#' 
#' \dontrun{ a <- calcOutput(type="FAO_fishery")
#' }
#' @importFrom magclass add_dimension new.magpie collapseNames dimSums getNames
#' @importFrom madrat readSource  calcOutput  
#' @export
calcFishCCImpactCheung2018 <- function(subtype){
  if (subtype == "General"){ # Reference Year (e.g. BAU, 2010)
    x_General <- readCheung2018(subtype = "General")
    #Conversion from mg C day^(-1) m2^(-1) to  tCyr^(-1)^km2^(-1)
    #10^-9 is for mg to t, 10^6 is for  m2 to km2 
    x_PrimProdintCyrkm2 <- x_General[,,"PrimProdinmgCday"] * (10^-9) * 365 * 10^6
    
    return(list(
      x=x_PrimProdintCyrkm2,
      weight=NULL,
      unit="fish production in Mt DM",
      description="fish production in Mt DM"))
    
  }
  else if (subtype=="ModelOutputDBEM"){
    x_ModelOutputDBEM <- readCheung2018(subtype = "ModelOutputDBEM")
    x_General <- readCheung2018(subtype = "General")
    x_PrimProdintCyrkm2 <- x_General[,,"PrimProdinmgCday"] * (10^-9) * 365 * 10^6
    w = x_PrimProdintCyrkm2 * x_General[,,"ExclEconZoneAreainkm2"]
    
    relationmatrix <- read.csv("/p/projects/rd3mod/inputdata/sources/Cheung2018/mappingEEZFAOfishingarea.csv")
    relationmatrix <- data.frame(lapply(relationmatrix, function(x) { gsub("\\(|\\)|\\,|\\-|\\’", "", x)}))
    relationmatrix <- data.frame(lapply(relationmatrix, function(x) { gsub(" +", " ", x)}))
    x_ModelOutputDBEM <- x_ModelOutputDBEM *10^-2
    x_ModelOutputDBEM[is.na(x_ModelOutputDBEM)] <- 0
    x_ModelOutputDBEM <- toolAggregate(x_ModelOutputDBEM, rel=relationmatrix, from="Scenarios", to="ï..FAO.Fishing.Area", weight=w, dim=3.1)
    
    #x_ModelOutputDBEM <- readCheung2018(subtype="ModelOutputDBEM")
    #x_ModelOutputDBEM <- convertCheung2018(subtype="ModelOutputDBEM")
    
    x_capture_marine_raw = calcOutput("FAO_fishery",subtype="capture_marine",aggregate = FALSE)
    x_aqua_raw <-  calcOutput("FAO_fishery",subtype="aquaculture",aggregate = FALSE)
    inland_waters <- c("Asia Inland waters","Europe Inland waters","Africa Inland waters",
                       "Oceania Inland waters","America North Inland waters",
                       "America South Inland waters")
    x_capture_marine_raw <- x_capture_marine_raw[,,inland_waters, invert=T]
    x_aqua_raw <- x_aqua_raw[,,inland_waters, invert=T]
    
    #or:x_capture_marine_raw <- x_capture_marine
    #extract relevant year
    #x_capture_marine <-  dimSums(x_capture_marine_raw[,"y2010",], dim=c(1,3.1))
   # x_capture_marine_raw <- x_capture_marine
    x_capture_marine <-  dimSums(x_capture_marine_raw[,"y2010",], dim=c(3.1))
    x_capture_marine_cc <- x_capture_marine + (x_capture_marine*setYears(x_ModelOutputDBEM,NULL))
    #x_total production needs to include x_all_capture and x_aqua_new
    
    #x_total_production_raw = calcOutput("FAO_fishery",subtype="all_capture",aggregate = FALSE) + calcOutput("FAO_fishery",subtype="aquaculture",aggregate = FALSE)
    #x_total_production_raw <- x_total_production_raw[,,"Mediterranean and Black Sea"]
    #OR: 
    #x_all_capture <- dimSums(x_all_capture, dim=3)
    #x_aqua_new <- dimSums(x_aqua_new, dim=c(3))
    #x_production_raw <- x_all_capture + x_aqua_new
    x_aqua_raw <- dimSums(x_aqua_raw, dim=3)
    x_capture_marine_raw <- dimSums(x_capture_marine_raw, dim=3)
    x_total_production_raw= x_capture_marine_raw + x_aqua_raw
    #x_total_production <- x_all_capture + dimSums(x_aqua_new, dim=3.3)
    #x_total_production_raw <- x_production_raw
    x_total_production <- dimSums(x_total_production_raw[,"y2010",], dim=3)
    #or: x_total_production_raw = dimSums(x_all_capture[,"y2010",], dim=3) + dimSums(x_aqua_new[,"y2010",], dim=3)
    
    x_capture_marine <- dimSums(x_capture_marine_raw[,"y2010",], dim=3.1)
    
    #x_marine_production_difference_cc <- x_capture_marine - x_capture_marine_cc
    x_capture_marine <- dimSums(x_capture_marine, dim=3)
    x_capture_marine_cc <- dimSums(x_capture_marine_cc, dim=3.1)
    x_total_production_cc <-  x_total_production - x_capture_marine + x_capture_marine_cc 
    
    x_production_raw <- dimSums(x_total_production_raw[,"y2010",], dim=3)
    
    #fulldim x_total_production_cc iso, variable y2010, data (4scenarios)
    #fulldim x_total_production iso, variable, data (NULL)
    #fulldim x_capture_marine iso, variable, data (NULL)
    #fulldim x_capture_marine_cc iso, variable, data (4scenarios)
    
    deltaDBEM = x_total_production_cc/x_production_raw
    deltaDBEM[is.na(deltaDBEM)] <- 1
    
    
    return(list(
      x=deltaDBEM,
      weight=x_total_production,
      unit="Change of fish production in Mt DM",
      description="Climate impacts on fish production based on estimates by Cheung et al 2018."))
    
  } else if (subtype=="ModelOutputDynModel"){
    
    x_ModelOutputDynModel <- readCheung2018(subtype = "ModelOutputDynModel")
    x_General <- readCheung2018(subtype = "General")
    x_PrimProdintCyrkm2 <- x_General[,,"PrimProdinmgCday"] * (10^-9) * 365 * 10^6
    w = x_PrimProdintCyrkm2 * x_General[,,"ExclEconZoneAreainkm2"]
    
    relationmatrix <- read.csv("/p/projects/rd3mod/inputdata/sources/Cheung2018/mappingEEZFAOfishingarea.csv")
    relationmatrix <- data.frame(lapply(relationmatrix, function(x) { gsub("\\(|\\)|\\,|\\-|\\’", "", x)}))
    relationmatrix <- data.frame(lapply(relationmatrix, function(x) { gsub(" +", " ", x)}))
    x_ModelOutputDynModel <- x_ModelOutputDynModel *10^-2
    x_ModelOutputDynModel[is.na(x_ModelOutputDynModel)] <- 0
    x_ModelOutputDynModel <- toolAggregate(x_ModelOutputDynModel, rel=relationmatrix, from="Scenarios", to="ï..FAO.Fishing.Area", weight=w, dim=3.1)
    
    #x_ModelOutputDynModel <- readSource("Cheung2018", subtype = "x_ModelOutputDynModel", convert=T)
    x_capture_marine_raw = calcOutput("FAO_fishery",subtype="capture_marine",aggregate = FALSE)
    x_aqua_raw <-  calcOutput("FAO_fishery",subtype="aquaculture",aggregate = FALSE)
    inland_waters <- c("Asia Inland waters","Europe Inland waters","Africa Inland waters",
                       "Oceania Inland waters","America North Inland waters",
                       "America South Inland waters")
    x_capture_marine_raw <- x_capture_marine_raw[,,inland_waters, invert=T]
    x_aqua_raw <- x_aqua_raw[,,inland_waters, invert=T]
    
    #or:x_capture_marine_raw <- x_capture_marine
    #extract relevant year
    #x_capture_marine <-  dimSums(x_capture_marine_raw[,"y2010",], dim=c(1,3.1))
    x_capture_marine <-  dimSums(x_capture_marine_raw[,"y2010",], dim=c(3.1))
    x_capture_marine_cc <- x_capture_marine + (x_capture_marine*setYears(x_ModelOutputDynModel,NULL))
    #x_total production needs to include x_all_capture and x_aqua_new
    
    x_aqua_raw <- dimSums(x_aqua_raw, dim=3)
    x_capture_marine_raw <- dimSums(x_capture_marine_raw, dim=3)
    x_total_production_raw= x_capture_marine_raw + x_aqua_raw
    #OR: 
    #x_all_capture <- dimSums(x_all_capture, dim=3)
    #x_aqua_new <- dimSums(x_aqua_new, dim=c(3))
    #x_production_raw <- x_all_capture + x_aqua_new
    
    #x_total_production <- x_all_capture + dimSums(x_aqua_new, dim=3.3)
    #x_total_production_raw <- x_production_raw
    x_total_production <- dimSums(x_total_production_raw[,"y2010",], dim=3)
    #or: x_total_production_raw = dimSums(x_all_capture[,"y2010",], dim=3) + dimSums(x_aqua_new[,"y2010",], dim=3)
    
    x_capture_marine <- dimSums(x_capture_marine_raw[,"y2010",], dim=3.1)
    
    #x_marine_production_difference_cc <- x_capture_marine - x_capture_marine_cc
    x_capture_marine <- dimSums(x_capture_marine, dim=3)
    x_capture_marine_cc <- dimSums(x_capture_marine_cc, dim=3.1)
    x_total_production_cc <-  x_total_production - x_capture_marine + x_capture_marine_cc 
    
    x_production_raw <- dimSums(x_total_production_raw[,"y2010",], dim=3)
    
    #fulldim x_total_production_cc iso, variable y2010, data (4scenarios)
    #fulldim x_total_production iso, variable, data (NULL)
    #fulldim x_capture_marine iso, variable, data (NULL)
    #fulldim x_capture_marine_cc iso, variable, data (4scenarios)
    
    deltaDynMod = x_total_production_cc/x_production_raw
    deltaDynMod[is.na(deltaDynMod)] <- 1
    
    return(list(
      x=deltaDynMod,
      weight=x_total_production,
      unit="Change of fish production in Mt DM",
      description="Climate impacts on fish production based on estimates by Cheung et al 2018."))
    
  }} 
