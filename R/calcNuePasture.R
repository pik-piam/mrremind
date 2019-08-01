#' @title calcNuePasture
#' @description calculates the soil nitrogen uptake efficiency of pastures. This is the nitrogen taken up from the soil (N in crop biomass minus biological fixation minus seed N) divided by the soil N inputs (fertilizer, manure etc). For the future, NUE scenarios are added.
#' @param cellular cellular disaggreagation or national values
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcSNUpE}}
#' \code{\link{calcNitrogenBudgetPasture}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NuePasture")
#' }
#' 



calcNuePasture<-function(cellular=FALSE){
  a<-calcOutput("NitrogenBudgetPasture",aggregate = F,cellular=cellular,deposition="Nsurplus2")
  outputs<-c(
    "harvest")
  inputs<-c(
    "fixation_freeliving",
    "grazing","fertilizer","deposition",
    "balanceflow")
  outputs<-dimSums(a[,,outputs],dim=3.1)
  inputs<-dimSums(a[,,inputs],dim=3.1)
  NUE<-outputs/inputs
  
  #future
  data<-toolNUEscenarios(x=NUE,weight=inputs)
  weight=data$weight
  out=data$x
  
  return(list(
    x=out,
    weight=weight,
    unit="Share",
    description="Soil nitrogen uptake efficiency",
    isocountries=!cellular))
}



