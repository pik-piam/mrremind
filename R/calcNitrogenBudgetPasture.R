#' @title calcNitrogenBudgetPasture
#' @description Calculates Nitrogen Budgets for Pasture soils on country levels.
#' @param include_fertilizer including fertilizer in budget. Use FALSE to avoid circularities in specific cases
#' @param deposition if FALSE, deposition is not accounted for in the distribution. Use FALSE to avoid circularities in calcNitrogenBudget
#' @param max_nue NULL or a numeric value. if numeric, an additional N balanceflow is included that takes care that the nitrogen use efficiency does not exceed the numeric value in balanceflow.
#' @param cellular cellular disaggreagation or national values
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenBudgetPasture")
#' }
#' @importFrom magclass setNames



calcNitrogenBudgetPasture<-function(cellular=FALSE,include_fertilizer=TRUE,deposition="CEDS",max_nue=0.9){
  past<-findset("past")
  
  harvest<-collapseNames(calcOutput("Production",products="pasture",cellular=cellular,aggregate = FALSE)[,,"nr"])
  excretion<-collapseNames(dimSums(calcOutput("Excretion",cellular=cellular,aggregate = FALSE)[,,"grazing"][,,"nr"],dim=3.2))
  fixation<-collapseNames(calcOutput("NitrogenBNF",cellular=cellular,aggregate = FALSE)[,,"past"])
  if(include_fertilizer==TRUE){
    fertilizer<-calcOutput("FertN",aggregate = F,appliedto="past",cellular=cellular,deposition=deposition,max_snupe=max_nue)[,past,]  
    fertilizer<-setNames(fertilizer,"fertilizer")
  } else {
    fertilizer=NULL
  }
  # som missing
  
  adeposition<-setNames(
    collapseNames(dimSums(calcOutput("AtmosphericDeposition",datasource=deposition,cellular=cellular,aggregate = FALSE)[,past,"past"],dim=c(3.4)))
      ,"deposition")
  if(!cellular) adeposition["ATA",,]<-0  ### Antarctica has large deposition but no icefree land
  
  outputs<-mbind(
    setNames(harvest,"harvest")
  )

  inputs<-mbind(
    setNames(fixation,"fixation_freeliving"),
    setNames(excretion,"grazing"),
    setNames(adeposition,"deposition"),  
    fertilizer
    )
  
  # Balanceflow based on assumption that everything above max_nue on country level is definetly a bug
  # For cellular calculation country data has to be loaded to scale down the balance flow
  if(!is.null(max_nue)){
    balanceflow<-(dimSums(outputs,dim=3.1))/max_nue-dimSums(inputs,dim=3.1)
    balanceflow[balanceflow<0]<-0
    
    if(cellular){
      iso           <- calcOutput("NitrogenBudgetPasture", cellular=FALSE, include_fertilizer=include_fertilizer, 
                                                            deposition=deposition, max_nue=max_nue, aggregate = FALSE)[,,"balanceflow"]
      CountryToCell <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
      balanceflow   <- toolAggregate(x=toolIso2CellCountries(iso), rel=CountryToCell, weight = balanceflow, from="iso", to="celliso")
    }
    
  } else {
    balanceflow<-dimSums(outputs,dim=3.1)*0
  }
#  balanceflow[,,]=0
  balanceflow<-setNames(balanceflow,"balanceflow")
  surplus<-setNames(dimSums(inputs,dim=3)+dimSums(balanceflow,dim=3)-dimSums(outputs,dim=3),"surplus")
  out<-mbind(outputs,inputs,balanceflow,surplus)
  
  
  #dimSums(outputs,dim=c(1,3))/dimSums(inputs,dim=c(1,3))
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr",
    description="Nitrogen budget on pastures for historical period",
    isocountries=!cellular))
}
