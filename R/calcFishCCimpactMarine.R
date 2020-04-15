#' @title calcFishCCimpactMarine
#' @description Derive relative change of marine fish production (capture AND marine aquaculture) based on scenarios by Cheung et al 2018.
#' @return Data for each FAO Major Fishing area
#' @param subtype "ModelOutputDBEM" DBEM Model output for RCP2.6;RCP8.5
#' "ModelOutputDynModel" Dynamic Model output for RCP2.6;RCP8.5
#' @param impacts_aquaculture TRUE marine aquaculture is assumed to have the same impacts as marine capture. FALSE only capture fishery is assumed to be impacted.
#' @author Jasmin Wehner, Benjamin Leon Bodirsky
#' @examples
#'
#' \dontrun{ a <- calcOutput(type="FishCCimpactMarine")
#' }
#' @importFrom magclass add_dimension new.magpie collapseNames dimSums getNames
#' @importFrom madrat readSource  calcOutput toolAggregate
#' @export
#' 
calcFishCCimpactMarine <- function(subtype, impacts_aquaculture=TRUE){
  if (subtype == "General"){ # Reference Year (e.g. BAU, 2010)
    x_General <- readSource("Cheung2018", subtype = "General",convert = FALSE)
    #Conversion from mg C day^(-1) m2^(-1) to  tCyr^(-1)^km2^(-1)
    #10^-9 is for mg to t, 10^6 is for  m2 to km2
    x_PrimProdintCyrkm2 <- x_General[,,"PrimProdinmgCday"] * (10^-9) * 365 * 10^6

    return(list(
      x=x_PrimProdintCyrkm2,
      weight=NULL,
      unit="fish production in Mt DM",
      description="fish production in Mt DM"))
  

  } else if (subtype%in%c("ModelOutputDynModel","ModelOutputDBEM")){

    #read in CC impacts
    marine_CC_impacts <- readSource("Cheung2018",subtype = subtype, convert = FALSE)
    marine_CC_impacts <- marine_CC_impacts *10^-2
    marine_CC_impacts[is.na(marine_CC_impacts)] <- 0
    
    #read in weight
    x_General <- readSource("Cheung2018",subtype = "General",convert = FALSE)
    x_PrimProdintCyrkm2 <- x_General[,,"PrimProdinmgCday"] * (10^-9) * 365 * 10^6
    w = x_PrimProdintCyrkm2 * x_General[,,"ExclEconZoneAreainkm2"]

    # aggregate marine_CC_impacts from EEZ to FAO fishing areas
    relationmatrix <- toolMappingFile(type = "regional",name = "FAOfishingarea2EEZ.csv",readcsv = T)
    marine_CC_impacts <- toolAggregate(marine_CC_impacts, rel=relationmatrix, from="ExclEconZone",
    to="FAO_Fishing_Area", weight=collapseNames(w),dim=3.1)

    # adding CC impacts for inland waters and set them to 0
    inland_waters <- c("Asia Inland waters","Europe Inland waters","Africa Inland waters",
                       "Oceania Inland waters","America North Inland waters",
                       "America South Inland waters")
    marine_CC_impacts <- add_columns(marine_CC_impacts,addnm = inland_waters,dim = 3.1)
    marine_CC_impacts[,,inland_waters] = 0
    
    
    # read in Fish production by FAO fishing areas, used as weights
    # capture fisheries
    x_capture_marine_raw = calcOutput("FAO_fishery", subtype="capture_marine", aggregate = FALSE)
    x_capture_marine <-  dimSums(x_capture_marine_raw[,"y2010",], dim=c(3.1))
    x_capture_marine_cc <- x_capture_marine + (x_capture_marine*marine_CC_impacts)
    
    # aquaculture
    x_aqua_raw <-  calcOutput("FAO_fishery", subtype="aquaculture", aggregate = FALSE)
    x_aqua_marine <- dimSums(x_aqua_raw,dim=3.1)[,"y2010",]
    x_aqua_marine_cc <- x_aqua_marine + (x_aqua_marine * marine_CC_impacts)
    
    # sum capture fsiheries and aquaculture. as names are not identical, we need to add it to the specific columns
    x_total_production = x_capture_marine + x_aqua_marine
    
    # apply CC impacts
    
    if(impacts_aquaculture==TRUE){
      
      x_total_production_cc = x_capture_marine_cc + x_aqua_marine_cc

    } else {
      
      x_total_production_cc = x_capture_marine_cc + x_aqua_marine
    
    }
    
    x_total_production_cc=dimSums(x_total_production_cc,dim="fishing_area")
    x_total_production=dimSums(x_total_production,dim="fishing_area")
    
    delta = x_total_production_cc/x_total_production
    delta[is.na(delta)] <- 1

    return(list(
      x=delta,
      weight=x_total_production,
      unit="Percentage change of dry matter fish production",
      description="Climate impacts on fish production based on estimates by Cheung et al 2018."))

  }
}
