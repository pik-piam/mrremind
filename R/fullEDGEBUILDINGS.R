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
#' 
fullEDGEBUILDINGS <- function(rev=0) {
  
  #-------------- socio-economic data -----------------------------------------------------------
  calcOutput("PopulationPast",aggregate=F, file = "f_poppast.cs4r")
  calcOutput("Population",FiveYearSteps = FALSE,aggregate=F, file = "f_pop.cs4r")
  calcOutput("GDPppp",FiveYearSteps = FALSE,aggregate=F, file = "f_gdp.cs4r")
 calcOutput("GDPpppPast",aggregate=F, file = "f_gdppast.cs4r")
 calcOutput("RatioPPP2MER", aggregate = F, file = "f_ppp2mer.cs4r")
 
 
 
 calcOutput("UrbanPast",aggregate=F, file = "f_urbanpast.cs4r")
 calcOutput("Urban",aggregate=F, file = "f_urban.cs4r")
 
 
 
 
 calcOutput("Surface",aggregate=F, file = "f_surface.cs4r")
 #-------------- energy -----------------------------------------------------------
 calcOutput("IO",subtype = "output_EDGE_buildings", aggregate = F, file = "f_edge_buildings.cs4r")
 calcOutput("IO",subtype = "output_EDGE", aggregate = F, file = "f_edge_stationary.cs4r")
 
 calcOutput("IEA_PFU", aggregate = F, file = "f_iea_pfu.cs4r")
 #-------------- climate data -----------------------------------------------------------
calcOutput("HDD_CDD", tlimit = "17", aggregate = F, file = "f_hddcdd_17.cs4r")
calcOutput("HDD_CDD", tlimit = "18", aggregate = F, file = "f_hddcdd_18.cs4r")
calcOutput("HDD_CDD", tlimit = "19", aggregate = F, file = "f_hddcdd_19.cs4r")
calcOutput("HDD_CDD", tlimit = "20", aggregate = F, file = "f_hddcdd_20.cs4r")
calcOutput("HDD_CDD", tlimit = "21", aggregate = F, file = "f_hddcdd_21.cs4r")
calcOutput("HDD_CDD", tlimit = "22", aggregate = F, file = "f_hddcdd_22.cs4r")
calcOutput("HDD_CDD", tlimit = "23", aggregate = F, file = "f_hddcdd_23.cs4r")
calcOutput("HDD_CDD", tlimit = "24", aggregate = F, file = "f_hddcdd_24.cs4r")
calcOutput("HDD_CDD", tlimit = "25", aggregate = F, file = "f_hddcdd_25.cs4r")

  #----------------------------------------------------------------------------------------------------  
  
}
