#' @title calcLanduse
#' @description Calculates the cellular MAgPIE landuse area based on LUH2v2 or LanduseInitialisation data.
#'
#' @param cellular cellular (TRUE) or country-level/regional (FALSE) data
#' @param landuse_types magpie (5 types) or LUH2v2, "ini_old" and "ini_new" for new and old implementation in calcLanduseInitialisation, carbon for CarbonBudget types (crop, past, urban, natveg)
#' @param selectyears defaults to past
#' @return List of magpie object with results on country or cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("Landuse")
#' }

calcLanduse <- function(cellular=FALSE, landuse_types="magpie", selectyears="past"){
  
  years <- sort(findset(selectyears,noset = "original"))
  
  if(landuse_types%in%c("magpie","LUH2v2")){

    Landuse <- calcOutput("LUH2v2", landuse_types=landuse_types, cellular=cellular, selectyears=selectyears, aggregate=FALSE)
    
  } else if(landuse_types=="carbon"){  
    
    Landuse <- calcOutput("LUH2v2", landuse_types="magpie", cellular=cellular, selectyears=selectyears, aggregate=FALSE)
    Landuse <- mbind(Landuse[,,c("crop","past","urban")], setNames(dimSums(Landuse[,,c("other","forest")], dim=3), nm="natveg"))

  } else if(landuse_types%in%c("ini_old","ini_new")){

    Landuse <- calcOutput("LanduseInitialisation", land = substring(landuse_types,5), cellular=cellular, selectyears=selectyears, aggregate=FALSE)
    
  } else { stop("Given landuse types (currently) not supported!")}
  
  if(cellular){Landuse <- toolCell2isoCell(Landuse)}
  
  return(list(x=Landuse,
              weight=NULL,
              unit="Mha",
              description=paste0("Land use data for ",landuse_types," land use set"),
              isocountries=!cellular)
  )
  
}
  
  