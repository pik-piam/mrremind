#' @importFrom magclass getRegionList<-
convertLUH2v1<-function(x,subtype){
  mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
  getRegionList(x) <- rep("GLO",ncells(x))
  
  out <- groupAggregate(data = x,query = mapping,from="cell",to="iso",dim=1)

  out  <- toolCountryFill(out,fill=0)
  return(out)
}  