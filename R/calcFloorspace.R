#' Floor space in buildings
#'
#' Residential, commercial and total floor space from EDGE-B. Set
#'
#' @author Antoine Levesque, Robin Hasse
#'
#' @param onlyTotal boolean, only give total instead of sub-sectoral floor space
#' @param scenario A string (or vector of strings) designating the scenario(s) to be returned.
#' @return MAgPIE object with buildings floor space
#'
#'
calcFloorspace <- function(scenario, onlyTotal = FALSE) {

  ## Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs", to calls of the individual scenarios.
  scenario <- mrdrivers::toolReplaceShortcuts(scenario) %>% unique()

  data <- readSource("EdgeBuildings", subtype = "Floorspace", subset = scenario)

  if (onlyTotal) {
    data <- collapseNames(data[, , "buildings"])
  }

  list(x = data,
       weight = NULL,
       unit = "million m2",
       description = "Buildings floor space")
}
