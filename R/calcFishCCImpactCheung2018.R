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
#' @importFrom madrat readSource  calcOutput toolAggregate
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
  

  } else if (subtype%in%c("ModelOutputDynModel","ModelOutputDBEM")){

    x_ModelOutputDynModel <- readSource("Cheung2018",subtype = subtype, convert = FALSE)
    x_General <- readSource("Cheung2018",subtype = "General",convert = FALSE)
    
    x_PrimProdintCyrkm2 <- x_General[,,"PrimProdinmgCday"] * (10^-9) * 365 * 10^6
    w = x_PrimProdintCyrkm2 * x_General[,,"ExclEconZoneAreainkm2"]

    relationmatrix <- toolMappingFile(type = "regional",name = "FAOfishingarea2EEZ.csv",readcsv = T)
    
    x_ModelOutputDynModel <- x_ModelOutputDynModel *10^-2
    x_ModelOutputDynModel[is.na(x_ModelOutputDynModel)] <- 0
    x_ModelOutputDynModel <- toolAggregate(x_ModelOutputDynModel, rel=relationmatrix, from="ExclEconZone",
    to="FAO_Fishing_Area", weight=collapseNames(w),dim=3.1)
    
    x_capture_marine_raw = calcOutput("FAO_fishery",subtype="capture_marine",aggregate = FALSE)
    x_aqua_raw <-  calcOutput("FAO_fishery",subtype="aquaculture",aggregate = FALSE)
    inland_waters <- c("Asia Inland waters","Europe Inland waters","Africa Inland waters",
                       "Oceania Inland waters","America North Inland waters",
                       "America South Inland waters")

    x_aqua_raw <- x_aqua_raw[,,inland_waters, invert=T]

    x_capture_marine <-  dimSums(x_capture_marine_raw[,"y2010",], dim=c(3.1))
    x_capture_marine_cc <- x_capture_marine + (x_capture_marine*setYears(x_ModelOutputDynModel,NULL))

    x_aqua_raw <- dimSums(x_aqua_raw, dim=3)
    x_capture_marine_raw <- dimSums(x_capture_marine_raw, dim=3)
    x_total_production_raw= x_capture_marine_raw + x_aqua_raw

    x_total_production <- dimSums(x_total_production_raw[,"y2010",], dim=3)

    x_capture_marine <- dimSums(x_capture_marine_raw[,"y2010",], dim=3.1)

    x_capture_marine <- dimSums(x_capture_marine, dim=3)
    x_capture_marine_cc <- dimSums(x_capture_marine_cc, dim=3.1)
    x_total_production_cc <-  x_total_production - x_capture_marine + x_capture_marine_cc

    x_production_raw <- dimSums(x_total_production_raw[,"y2010",], dim=3)


    delta = x_total_production_cc/x_production_raw
    delta[is.na(delta)] <- 1

    return(list(
      x=delta,
      weight=x_total_production,
      unit="Change of fish production in Mt DM",
      description="Climate impacts on fish production based on estimates by Cheung et al 2018."))

  }
}
