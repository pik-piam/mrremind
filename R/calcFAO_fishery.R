#' @title calcFAO_fishery
#' @description calculates fish data as Mt dry matter 
#' @param subtype "all_capture" takes all data into account that has been listed as capture fishery
#' "capture_marine" takes all fishdata into account that has been captured in marine fishing areas
#' "aquaculture" takes all data into account that has been listed as aquaculture fishery
#' @return Magpie object with fish data in dry matter
#' @author Jasmin Wehner, Benjamin Leon Bodirsky
#' @importFrom madrat readSource  calcOutput 
#' @importFrom magclass collapseNames
#' @export

calcFAO_fishery <- function(subtype="all_capture"){
  
  x_all_capture = readSource("FAO_fishery",subtype="capture",convert=TRUE)
 
  wm<-calcOutput("Attributes",aggregate = F)[,,"fish"][,,"wm"]
  #unit conversion from t to Mio tonnes and dry matter conversion
  x_all_capture <- x_all_capture/10^6/collapseNames(wm)
  #fish_production <- x_capture
  
  return(list(
    x=x_all_capture,
    weight=NULL,
    unit="Mt DM",
    description="Capture fishery production"))
  
  if (subtype=="capture_marine"){
    
    x_capture_marine = readSource("FAO_fishery",subtype="capture",convert=TRUE)
    
    #removal of inland waters as they are not considered in Exclusive Economic Zones 
    inland_waters <- c("Asia Inland waters","Europe Inland waters","Africa Inland waters",
                       "Oceania Inland waters","America North Inland waters",
                       "America South Inland waters")
    x_capture_marine <- x_capture_marine[,,inland_waters, invert=T]
    
    wm<-calcOutput("Attributes",aggregate = F)[,,"fish"][,,"wm"]
    #unit conversion from t to Mio tonnes and dry matter conversion
    x_capture_marine <- x_capture_marine/10^6/collapseNames(wm)
    
    return(list( 
      x=x_capture_marine,
      weight=NULL,
      unit="Mt DM",
      description="Marine capture fishery production"))
    
    
  } else if (subtype=="aquaculture"){
    
    x_aqua <-readSource("FAO_fishery", subtype="aquaculture", convert = T)
    wm<-calcOutput("Attributes",aggregate = F)[,,"fish"][,,"wm"]
    #unit conversion from t to Mio t 
    x_aqua_new <- x_aqua/10^6/collapseNames(wm)
    
    return(list(
      x=x_aqua_new,
      weight=NULL,
      unit="Mt DM",
      description="Aquaculture fishery production"))
    
    
  }
}
