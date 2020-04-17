#' @title calcFishCCimpactMarine
#' @description Derive relative change of marine fish production (capture AND marine aquaculture) based on scenarios by Cheung et al 2018.
#' @return Data for each FAO Major Fishing area
#' @param impactmodel "ModelOutputDBEM" DBEM Model output for RCP2.6;RCP8.5
#' "ModelOutputDynModel" Dynamic Model output for RCP2.6;RCP8.5
#' @param impacts fish types being affected by CC impacts. So far only available for "marine_capture" and "marine_aquaculture"
#' @param total if TRUE, aggregated to total production over all fish types
#' @author Jasmin Wehner, Benjamin Leon Bodirsky
#' @examples
#'
#' \dontrun{ a <- calcOutput(type="FishCCimpactMarine")
#' }
#' @importFrom magclass add_dimension new.magpie collapseNames dimSums getNames
#' @importFrom madrat readSource  calcOutput toolAggregate
#' @export
#' 
calcFishCCimpacts <- function(impactmodel="ModelOutputDBEM", impacts=c("marine_capture","marine_aquaculture"),total=FALSE){
  
  if (!all(impacts%in%c("marine_capture","marine_aquaculture"))) {
    stop("climate impacts so far can only be activated for marine systems")
  }
  
  #read in CC impacts
  marine_CC_impacts <- 1+ readSource("Cheung2018",subtype = impactmodel, convert = "onlycorrect")
  
  #read in weight
  x_General <- readSource("Cheung2018",subtype = "General",convert = "onlycorrect")
  w = x_General[,,"PrimProdMtYr"]  * x_General[,,"ExclEconZoneAreainkm2"]

  # aggregate marine_CC_impacts from EEZ to FAO fishing areas
  relationmatrix <- toolMappingFile(type = "regional",name = "FAOfishingarea2EEZ.csv",readcsv = T)
  marine_CC_impacts <- toolAggregate(marine_CC_impacts, rel=relationmatrix, from="ExclEconZone",
  to="FAO_Fishing_Area", weight=collapseNames(w),dim=3.1)

  # read in Fish production by FAO fishing areas, used as weights
  # capture fisheries
  production = calcOutput("FAO_fishery", by_fishing_area=TRUE, aggregate = FALSE)
  production <-  setYears(production[,"y2010",],NULL)
  
  # apply CC impacts
  
  # creating an object of the right dimensionality
  tmp=dimSums(marine_CC_impacts,dim="data") * 0+1
  production_cc = production * tmp
  
  inland_waters <- c("Asia Inland waters","Europe Inland waters","Africa Inland waters",
                     "Oceania Inland waters","America North Inland waters",
                     "America South Inland waters")
  marine_waters <- setdiff(getNames(production,dim=2),inland_waters)
  
  if("marine_capture"%in%impacts) {
    production_cc[,,"capture"][,,marine_waters] = production_cc[,,"capture"][,,marine_waters] * marine_CC_impacts
  }
  if("marine_aquaculture"%in%impacts) {
    production_cc[,,"aquaculture"][,,marine_waters] = production_cc[,,"aquaculture"][,,marine_waters] * marine_CC_impacts
  }
  
  if(total==TRUE){
    total_production = dimSums(production,dim="fishing_area")
    total_production_cc = dimSums(production_cc,dim="fishing_area")
    
  } else {
    prod_inland = add_dimension( dimSums(production[,,inland_waters],dim="fishing_area"),dim = 3.2,add = "fishing_area_type",nm = "inland_waters" )
    prod_marine = add_dimension( dimSums(production[,,inland_waters,invert=TRUE],dim="fishing_area"),dim = 3.2,add = "fishing_area_type",nm = "marine_waters" )
    total_production<-mbind( prod_inland,prod_marine)
    
    prod_inland_cc = add_dimension( dimSums(production_cc[,,inland_waters],dim="fishing_area"),dim = 3.2,add = "fishing_area_type",nm = "inland_waters" )
    prod_marine_cc = add_dimension( dimSums(production_cc[,,inland_waters,invert=TRUE],dim="fishing_area"),dim = 3.2,add = "fishing_area_type",nm = "marine_waters" )
    total_production_cc<-mbind( prod_inland_cc,prod_marine_cc)
  }
  
  #colSums(total_production_cc)/colSums(total_production)
  
  delta = total_production_cc/total_production
  
  delta[is.na(delta)] <- 1

  return(list(
    x=delta,
    weight=total_production,
    unit="Percentage change of dry matter fish production",
    description="Climate impacts on fish production based on estimates by Cheung et al 2018."))


}
