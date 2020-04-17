#' @title calcFAO_fishery
#' @description calculates fish data as Mt dry matter, distinguishing capture and aquaculture as well as fishing areas
#' @param by_fishing_area if TRUE, all fishing areas are provided. if FALSE, only marine and inlandwaters are distinguished
#' @return Magpie object with fish data in dry matter
#' @author Jasmin Wehner, Benjamin Leon Bodirsky
#' @importFrom madrat readSource  calcOutput
#' @importFrom magclass collapseNames
#' @export

calcFAO_fishery <- function(by_fishing_area = FALSE){

  capture = readSource("FAO_fishery",subtype="capture",convert=TRUE)
  aquaculture = readSource("FAO_fishery",subtype="aquaculture",convert=TRUE)
  
  capture = add_dimension( dimSums(capture,dim=c("fish_category")) ,dim = 3.1,add = "fishing_type",nm = "capture" )
  aquaculture = add_dimension( dimSums(aquaculture,dim=c("fish_category","environment")) ,dim = 3.1,add = "fishing_type",nm = "aquaculture" )
  
  
  total = mbind(
    capture, aquaculture
  )
  
  if (by_fishing_area==FALSE) {
    inland_waters <- c("Asia Inland waters","Europe Inland waters","Africa Inland waters",
                       "Oceania Inland waters","America North Inland waters",
                       "America South Inland waters")
    inland = add_dimension( dimSums(total[,,inland_waters],dim="fishing_area"),dim = 3.2,add = "fishing_area_type",nm = "inland_waters" )
    marine = add_dimension( dimSums(total[,,inland_waters,invert=TRUE],dim="fishing_area"),dim = 3.2,add = "fishing_area_type",nm = "marine_waters" )
    total<-mbind( inland,marine)
      
  } else if (by_fishing_area!=TRUE) {stop("by_fishing_area has to be boolean")}
  
  return(list(
    x=total,
    weight=NULL,
    unit="Mt DM",
    description="Fishery production"))

}
