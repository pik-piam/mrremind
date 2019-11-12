#' @title calcNitrogenSurplusByCrop
#' @description calculates the crop-specific nitrogen losses and the balanceflow for countries with unrealistically high nitrogen uptake efficiencies
#'
#' @param indicator total: estimates the inputs per total crop production; by_area estimates the inputs per area harvested
#' @param deposition if FALSE, deposition is not accounted for in the distribution. Use FALSE to avoid circularities in calcNitrogenBudget
#' @param cellular cellular disaggreagation or national values
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenSurplusByCrop")
#' }
#' @importFrom magpiesets findset



calcNitrogenSurplusByCrop<-function(indicator="total",deposition="Nsurplus2",cellular=FALSE){
  past<-findset("past")
  nb<-calcOutput("NitrogenBudgetCropland",deposition=deposition,cellular=cellular,aggregate = FALSE)
  
  withdrawal=calcOutput("NitrogenWithdrawalByCrop",cellular=cellular,aggregate=FALSE,indicator="total")
  withdrawal=dimSums(withdrawal,dim=3.1)
  inputs<-nb[,,c("surplus","balanceflow")]  
  
  inputs_per_crop=inputs/dimSums(withdrawal,dim=3.1)*withdrawal
  if (indicator=="by_physical_area"){
    area<-collapseNames(calcOutput("Croparea",aggregate = FALSE,physical=TRUE,cellular=cellular,sectoral="kcr")[,past,])
    out<-inputs_per_crop[,,getNames(area)]/area
    weight<-out
    weight[,,]<-area
    data<-toolNAreplace(x=out,weight=weight)
    weight=data$weight
    out=data$x
  } else if (indicator=="by_area_harvested"){
    area<-collapseNames(calcOutput("Croparea",physical=FALSE,cellular=cellular,aggregate = FALSE,sectoral="kcr")[,past,])
    out<-inputs_per_crop[,,getNames(area)]/area
    weight<-out
    weight[,,]<-area
    data<-toolNAreplace(x=out,weight=weight)
    weight=data$weight
    out=data$x
    unit="t Nr per ha area harvested"
  } else if (indicator=="total") {
    out<-inputs_per_crop
    weight=NULL
    out[is.na(out)]<-0
    out[is.nan(out)]<-0
    unit="Mt Nr"
  } else {stop("unknown indicator")}
  
  return(list(x=out,
              weight=weight,
              unit=unit,
              description="Nitrogen inputs by crop type",
              isocountries =!cellular
              ))
}