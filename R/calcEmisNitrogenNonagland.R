#' @title calcEmisNitrogenNonaglandPast
#' @description 
#' Calculates nitrogenous emissions from non-agricultural land for the historical period
#' @param method Method for calculating Atmospheric deposition: Nsurplus2 and Nsurplus are based on deposition rates based on own emission calculations after 2 or after 1 iteration, respectively.
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcEmisNitrogenPast}},
#' \code{\link{calcExcretion}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenPasturePast")
#' }
#' 


calcEmisNitrogenNonaglandPast<-function(method="Nsurplus"){
  
  # first iteration: calculate atmospheric deposition based on CEDS and estimate leaching
  # second iteration: calculate deposition based on Nsurplus and Oceans based on leaching    
  if(method=="Nsurplus2"){
    budget<-calcOutput("NitrogenBudgetNonagland",aggregate=FALSE,deposition="Nsurplus")
    method="Nsurplus"
  } else {
    budget<-calcOutput("NitrogenBudgetNonagland",aggregate=FALSE,deposition="CEDS")
  }
  

  # apply preagshare on nonagland
  
  preag=calcOutput("EmisNitrogenPreagriculture",aggregate = FALSE,cellular=FALSE)
  preag=preag[,,c("crop","past"),invert=TRUE]
  preagshr=preag/dimSums(preag,dim=3.1)
  
  out<-budget[,,"surplus"]*preagshr
  out<-dimSums(out,dim=c(3.1,3.2))
  out<-add_dimension(out,dim = 3.1,nm = "nonag_soils")
  
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr in various forms",
    description="Nitrogen emissions from non-agricultural land for the historical period"))
}