#' @importFrom magclass getRegionList<- nregions
calcSOMlossN<-function(cellular=FALSE){
  SOM <- calcOutput("SOM",aggregate = FALSE)
  SOM = -SOM[,,"delta_soilc"][,,"cropland"]/15
  

  if(cellular==FALSE){
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
    SOM  <- toolAggregate(SOM,rel = mapping,from=ifelse(nregions(SOM)>1,"celliso","cell"),to="iso",dim=1)
    SOM  <- toolCountryFill(SOM,fill=0)
  }
  
  return(list(
    x=SOM,
    weight=NULL,
    unit="Mt Nr",
    description="Nitrogen release or bounding due to changes in Soil Organic Matter",
    isocountries = !cellular))
}