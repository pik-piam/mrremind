#' @title calcSOCLossShare
#' @description Calculates soil organic carbon loss share on cellular level 
#' 
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("SOCLossShare", aggregate=FALSE)
#' }

calcSOCLossShare<- function(){
  
  years                  <- findset("past")
  KG_climate             <- readSource("Koeppen", subtype="cellular", convert="onlycorrect")[,years,]
  KG2IPCC                <- toolGetMapping("mapping_koeppen_ipcc.csv", type="sectoral")
  getNames(KG_climate)   <- tolower(getNames(KG_climate))
  KG2IPCC$koeppen_geiger <- tolower(KG2IPCC$koeppen_geiger)
  IPCC_climate           <- toolAggregate(KG_climate, rel=KG2IPCC, from = "koeppen_geiger", to = "ipcc_reduced", dim=3)
  
  SCFsub2IPCCclimate     <- readSource("IPCC",    subtype="SCF_sub" , convert=FALSE)[,,getNames(IPCC_climate)]
  SOCLossShare           <- dimSums(IPCC_climate * SCFsub2IPCCclimate, dim=3.1)
  
  return(list(
    x            = SOCLossShare ,
    weight       = NULL,
    unit         = "tC/tC",
    description  = "Soil organic carbon loss share per crop type",
    isocountries = FALSE))
}
