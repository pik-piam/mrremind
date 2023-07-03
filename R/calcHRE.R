#' Calculate Final Energy for the buildings sector from Heat Roadmap Europe scenarios
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
#' @importFrom dplyr select
#' @importFrom madrat toolGetMapping
#' @importFrom magclass as.magpie
#' @export

calcHRE <- function() {
  
  x <- readSource("HRE")
  
  # copy 2015 values of Baseline to other scenarios
  x[, "y2015", "HRE_ConvDecarb"]  <- x[, "y2015", "HRE_Baseline"]
  x[, "y2015", "HRE_HeatRoadmap"] <- x[, "y2015", "HRE_Baseline"]
  
  # map to REMIND variables
  map_sector <- toolGetMapping("Mapping_HRE_sectoral.csv", type="sectoral", where = "mappingfolder")
  x <- toolAggregate(x, dim=3.2, rel=map_sector, from = "HRE", to="REMIND", partrel = T, verbosity = 2)
  
  # multiply by conversion factors to convert from HRE (TWh/year) to REMIND (EJ/yr)
  # Warning: this conversion will not work if a variable occurs multiple times in the mapping with different 
  # conversion factors
  for (var in unique(map_sector$REMIND)) {
    x[,, var] <- x[,, var] * map_sector[map_sector$REMIND == var, "Conversion"][1]
  }
  getNames(x, dim=2) = paste0(getNames(x, dim=2), " (EJ/yr)")
  
  return(list(x = x, weight = NULL, unit = "EJ/yr",
         description = "FE scenarios from Heat Roadmap Europe"))
}
