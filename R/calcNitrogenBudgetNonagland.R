#' @title calcNitrogenBudgetNonagland
#' @description Calculates Nitrogen Budgets for Non-agricultural land on country levels.
#'
#' @param deposition if FALSE, deposition is not accounted for in the distribution. Use FALSE to avoid circularities in calcNitrogenBudget
#' @param max_nue NULL or a numeric value. if numeric, an additional N balanceflow is included that takes care that the nitrogen use efficiency does not exceed the numeric value in balanceflow.
#' @param cellular TRUE returns output on 0.5° grid
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenBudgetNonagland")
#' }
#' @importFrom magclass setNames



calcNitrogenBudgetNonagland<-function(deposition="CEDS",max_nue=0.95,cellular=FALSE){
  past<-findset("past")
  
  fixation<-calcOutput("NitrogenBNF",cellular=cellular,aggregate = FALSE)[,,c("past","crop"),invert=TRUE]
  
  deposition<-collapseNames(dimSums(calcOutput("AtmosphericDeposition",datasource=deposition, cellular=cellular, aggregate = FALSE)[,past,][,,c("past","crop"),invert=TRUE],dim=c(3.4)))
  
  inputs<-mbind(
    add_dimension(fixation,dim = 3.2,nm = "fixation_freeliving"),
    add_dimension(deposition,dim = 3.2,nm = "deposition")
    )
  
  outputs<-add_dimension(fixation,dim = 3.2,nm = "accumulation")
  outputs[,,]<-0
  
  # Balanceflow based on assumption that everything above max_nue on country level is definetly a bug
  if(!is.null(max_nue)){
    balanceflow<-(dimSums(outputs,dim=3.2))/max_nue-dimSums(inputs,dim=3.2)
    balanceflow[balanceflow<0]<-0
  } else {
    balanceflow<-dimSums(outputs,dim=3.2)*0
  }
#  balanceflow[,,]=0
  balanceflow<-add_dimension(balanceflow,dim = 3.2,nm = "balanceflow")
  surplus<-add_dimension(dimSums(inputs,dim=3.2)+dimSums(balanceflow,dim=3.2)-dimSums(outputs,dim=3.2),dim = 3.2,nm = "surplus")
  out<-mbind(outputs,inputs,balanceflow,surplus)
  
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr",
    description="Nitrogen budget on non-agricultural lands for historical period",
    isocountries =!cellular))
}
