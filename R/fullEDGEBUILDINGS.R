#' fullEDGEBUILDINGS
#' 
#' Function that produces the complete ISO data set required for the
#' EDGE - Buildings model.
#' 
#' @param rev data revision which should be used as input (positive numeric).
#' \code{\link{setConfig}} (e.g. for setting the mainfolder if not already set
#' properly).
#' @author Antoine Levesque
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' fullEDGEBUILDINGS()
#' }

fullEDGEBUILDINGS <- function(rev = 0) {
  #-------------- socio-economic data ------------------------------------------
  calcOutput("PopulationPast", aggregate = FALSE, file = "f_poppast.cs4r")
  calcOutput("Population",     aggregate = FALSE, file = "f_pop.cs4r", FiveYearSteps = FALSE)
  calcOutput("GDPppp",         aggregate = FALSE, file = "f_gdp.cs4r", FiveYearSteps = FALSE)
  calcOutput("GDPpppPast",     aggregate = FALSE, file = "f_gdppast.cs4r")
  calcOutput("RatioPPP2MER",   aggregate = FALSE, file = "f_ppp2mer.cs4r")
  calcOutput("UrbanPast",      aggregate = FALSE, file = "f_urbanpast.cs4r")
  calcOutput("Urban",          aggregate = FALSE, file = "f_urban.cs4r")
  calcOutput("Surface",        aggregate = FALSE, file = "f_surface.cs4r")

  #-------------- energy -------------------------------------------------------
  calcOutput("IO", subtype = "output_EDGE_buildings", aggregate = FALSE, file = "f_edge_buildings.cs4r")
  calcOutput("IO", subtype = "output_EDGE",           aggregate = FALSE, file = "f_edge_stationary.cs4r")
  calcOutput("IEA_PFU",                               aggregate = FALSE, file = "f_iea_pfu.cs4r")

  #-------------- climate data -------------------------------------------------
  for (tlim in as.character(17:25)) {
    calcOutput("HDD_CDD", tlimit = tlim, aggregate = FALSE, file = paste0("f_hddcdd_", tlim, ".cs4r"))
  }
}
