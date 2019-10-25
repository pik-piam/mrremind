#' @title calcLanduseChange
#' @description Calculates the cellular MAgPIE landuse change area based on LUH2v2 or LanduseInitialisation data.
#'
#' @param cellular cellular (TRUE) or country-level/regional (FALSE) data
#' @param landuse_types magpie (5 types) or LUH2v2, "ini_old" and "ini_new" for new and old implementation in calcLanduseInitialisation, carbon for CarbonBudget types (crop, past, urban, natveg)
#' @return List of magpie object with results on country or cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LanduseChange")
#' }

calcLanduseChange <- function(cellular=FALSE, landuse_types="magpie"){
  
  years <- findset("past")
  years <- c("y1960", years)
  
  if(landuse_types%in%c("magpie","LUH2v2")){
    
    Landuse <- calcOutput("LUH2v2", landuse_types=landuse_types, cellular=cellular, selectyears=years, aggregate=FALSE)
    
  } else if(landuse_types=="carbon"){  
 
    Landuse <- calcOutput("LUH2v2", landuse_types="magpie", cellular=cellular, selectyears=years, aggregate=FALSE)
    Landuse <- mbind(Landuse[,,c("crop","past","urban")], setNames(dimSums(Landuse[,,c("other","forest")], dim=3), nm="natveg"))

  } else if(landuse_types%in%c("ini_old","ini_new")){
    
    Landuse <- calcOutput("LanduseInitialisation", land = substring(landuse_types,5), cellular=cellular, selectyears=years, aggregate=FALSE)
    
  } else { stop("Given landuse types (currently) not supported!")}
  
  
  LanduseChange <- Landuse[,years[2:length(years)],]-setYears(Landuse[,years[1:length(years)-1],],years[2:length(years)])
  
  LandReduction <- LandExpansion <- LanduseChange
  LandReduction[LandReduction>0] <- 0
  LandReduction                  <- (-1)*LandReduction
  LandExpansion[LandExpansion<0] <- 0
  
  out <- mbind(add_dimension(LandReduction, dim=3.1, add="change", nm="reduction"), add_dimension(LandExpansion, dim=3.1, add="change", nm="expansion"))
  
  if(cellular){out <- toolCell2isoCell(out)}
  
  return(list(x=out,
              weight=NULL,
              unit="Mha",
              description=paste0("Land use change (reduction, expansion) for ",landuse_types," land use set"),
              isocountries=!cellular)
  )
  
}
  
  