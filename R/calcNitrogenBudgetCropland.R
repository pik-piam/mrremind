#' @title calcNitrogenBudgetCropland
#' @description Calculates Nitrogen Budgets for Cropland soils on country levels.
#'
#' @param deposition if FALSE, deposition is not accounted for in the distribution. Use FALSE to avoid circularities in calcNitrogenBudget
#' @param include_fertilizer including fertilizer in budget. Use FALSE to avoid circularities in specific cases
#' @param max_snupe NULL or a numeric value. if numeric, an additional N balanceflow is included that takes care that the soil nitrogen uptake efficiency does not exceed the numeric value in balanceflow.
#' @param cellular disaggregated to 0.5 degree grid
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenBudgetCropland")
#' }
#' @importFrom magclass setNames



calcNitrogenBudgetCropland<-function(cellular=FALSE,deposition="CEDS",include_fertilizer=TRUE,max_snupe=0.85){
  past<-findset("past")
  kcr<-findset("kcr")
  
  harvest<-dimSums(calcOutput("Production",products="kcr",cellular=cellular,calibrated=TRUE,aggregate = FALSE)[,,"nr"],dim=3)
  ag<-collapseNames(calcOutput("ResFieldBalancePast",aggregate = FALSE,cellular=cellular)[,,"nr"])
  bg<-dimSums(collapseNames(calcOutput("ResBiomass",cellular=cellular,plantparts="bg",aggregate = FALSE)[,,"nr"]),dim=3.1)
  
  seed<-dimSums(calcOutput("Seed",cellular=cellular,products="kcr",aggregate=FALSE)[,,"nr"],dim=3)
  
  fixation<-dimSums(calcOutput("NitrogenFixationPast",fixation_types="both",sum_plantparts=TRUE,aggregate = FALSE,cellular=cellular),dim=3.2)
  som<-calcOutput("SOMlossN",cellular=cellular,aggregate = F)[,past,]
  if(cellular){ som<-toolCell2isoCell(som)}
  if(include_fertilizer==TRUE){
    fertilizer<-calcOutput("FertN",aggregate = F,appliedto="crop",cellular=cellular,deposition=deposition,max_snupe=max_snupe)[,past,]  
    fertilizer<-setNames(fertilizer,"fertilizer")
  } else {
    fertilizer=NULL
  }
  
  manure<-collapseNames(calcOutput("ManureRecyclingCroplandPast",aggregate = FALSE,cellular=cellular)[,,"nr"])
  manure_cropland_grazing<-collapseNames(dimSums(calcOutput("Excretion",cellular=cellular,aggregate = FALSE)[,,"stubble_grazing"][,,"nr"],dim=3.2))
  adeposition<-setNames(collapseNames(
    dimSums(calcOutput("AtmosphericDeposition",datasource=deposition,cellular=cellular, aggregate = FALSE)[,past,"crop"],dim=c(3.4)))
    ,"deposition")
  if(!cellular) adeposition["ATA",,]<-0
  
  outputs<-mbind(
    setNames(harvest,"harvest"),
    setNames(collapseNames(ag[,,"biomass"]),"ag"),
    setNames(bg,"bg"))

  inputs_direct=mbind(
    setNames(seed,"seed"),
    setNames(fixation[,,"fixation_crops"],"fixation_crops")
  )
  
  inputs<-mbind(
    setNames(fixation[,,"fixation_freeliving"],"fixation_freeliving"),
    setNames(manure,"manure"),
    setNames(manure_cropland_grazing,"grazing"),
    setNames(collapseNames(ag[,,"recycle"]),"ag_recycling"),
    setNames(bg,"bg_recycling"),
    setNames(collapseNames(ag[,,"ash"]),"ag_ash"),
    setNames(adeposition,"deposition"),  
    setNames(som,"som"),
    fertilizer
    )
  
  # Balanceflow based on assumption that everything above max_snupe on country level is definetly a bug
  # For cellular calculation country data has to be loaded to scale down the balance flow
  if(!is.null(max_snupe)){
    balanceflow<-(dimSums(outputs,dim=3.1)-dimSums(inputs_direct,dim=3.1))/max_snupe-dimSums(inputs,dim=3.1)
    balanceflow[balanceflow<0]<-0
    
    if(cellular){
      iso           <- calcOutput("NitrogenBudgetCropland", cellular=FALSE, include_fertilizer=include_fertilizer, 
                                  deposition=deposition, max_snupe=max_snupe, aggregate = FALSE)[,,"balanceflow"]
      CountryToCell <- toolMappingFile(type="cell",name = "CountryToCellMapping.csv",readcsv = TRUE)
      balanceflow   <- toolAggregate(x=toolIso2CellCountries(iso), rel=CountryToCell, weight = balanceflow, from="iso", to="celliso")
    }
    
  } else {
    balanceflow<-dimSums(outputs,dim=3.1)*0
  }
#  balanceflow[,,]=0
  balanceflow<-setNames(balanceflow,"balanceflow")
  surplus<-setNames(dimSums(inputs,dim=3)+dimSums(inputs_direct,dim=3)+dimSums(balanceflow,dim=3)-dimSums(outputs,dim=3),"surplus")
  out<-mbind(outputs,inputs_direct,inputs,balanceflow,surplus)
  
  
  #dimSums(outputs,dim=c(1,3))/dimSums(inputs,dim=c(1,3))
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr",
    description="Nitrogen budget on croplands for historical period",
    isocountries =!cellular))
}

