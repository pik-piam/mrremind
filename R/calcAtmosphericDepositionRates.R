#' @title calcAtmosphericDepositionRates
#' @description Conputes Atmospheric (nitrogen) deposition rates per area on different land-use types.
#' @param cellular TRUE for results on 0.5 degree grid.
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcAtmosphericDeposition}},
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("AtmosphericDepositionRates")
#' }
#' 

calcAtmosphericDepositionRates<-function(cellular=FALSE){
  #dep<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",scenario=c("rcp26","rcp45","rcp85"),aggregate = FALSE)
  #  dep<-dimSums(dep,dim=c(3.3,3.4))
  
  dep<-calcOutput("AtmosphericDeposition",datasource="Nsurplus2",cellular=cellular,aggregate = FALSE)
  dep<-dimSums(dep,dim=c(3.3,3.4))
  luhdata<-calcOutput("LanduseInitialisation",cellular=cellular,aggregate=FALSE)
  dep <- toolHoldConstantBeyondEnd(dep)
  weight <- toolHoldConstantBeyondEnd(luhdata)
  
  out<-dep/weight
  out[is.na(out)]<-0
  out[is.infinite(out)]<-0
  
  return(list(
    x=out,
    weight=weight,
    unit="Mt Nr / Mha",
    min=0,
    max=2000,
    isocountries=(!cellular & (nregions(out)!=1)),
    description="Atmospheric deposition per ha on different land types."))
}