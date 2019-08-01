#' @title calcAreaEquippedForIrrigation
#' @description Calculates the area equipped for irrigation based on LU2v1 dataset. It assumes, that all cropland irrigated in the last 20 years at least once is equipped for irrigation.
#'
#' @param cellular if true, dataset is returned on 0.5 degree resolution
#' 
#' @return List of magpie objects with results on country/cellular level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcLanduseInitialisation}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("LUH2v2")
#' }
#' @importFrom magclass as.magpie getRegionList<- ncells
#' @importFrom luscale groupAggregate
#' 


calcAreaEquippedForIrrigation<-function(cellular=FALSE){
  x<-readSource("LUH2v2",subtype="irrigation",convert=FALSE)
  x<-dimSums(x,dim=3)
  past<-as.numeric(substring(findset("past"),2))
  out<-NULL
  for (year_x in past){
    span<-(year_x-20):year_x
    tmp<-setYears(as.magpie(apply(X = x[,span,],FUN = max,MARGIN = 1)),paste0("y",year_x))
    out<-mbind(out,tmp)
  }
  if (!cellular){
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
    getRegionList(out) <- rep("GLO",ncells(out))
    out <- groupAggregate(data=out,query = mapping,from="cell",to="iso",dim=1)
    out  <- toolCountryFill(out,fill=0)
  }
  
  return(list(
    x=out,
    weight=NULL,
    unit="Million ha",
    description="Million hectare land area for different land use types.",
    isocountries=!cellular))
}
