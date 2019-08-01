#' @title calcSNUpE
#' @description calculates the soil nitrogen uptake efficiency. This is the nitrogen taken up from the soil (N in crop biomass minus biological fixation minus seed N) divided by the soil N inputs (fertilizer, manure etc). For the future, SNuPE scenarios are added.
#' @param max_snupe Maximum realistic SNUPE. All values above will be limited to this value. Only holds for past values; future scneario values can exceed this number.
#' @param cellular disaggregated to 0.5 degree grid
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenBudgetCropland}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("SNUpE")
#' }
#' 



calcSNUpE<-function(max_snupe=0.85,cellular=FALSE){
  a<-calcOutput("NitrogenBudgetCropland",max_snupe=max_snupe,aggregate = F,deposition="Nsurplus2",cellular=cellular)
  a[,,"seed"]=-a[,,"seed"]
  a[,,"fixation_crops"]=-a[,,"fixation_crops"]
  a[,,"som"]=a[,,"som"]*1
  outputs<-c(
    "fixation_crops",
    "harvest","ag","bg","seed")
  inputs<-c(
    "fixation_freeliving",
    "som","fertilizer","deposition",
    "manure","grazing",
    "bg_recycling","ag_recycling",
    "ag_ash","balanceflow")
  outputs<-dimSums(a[,,outputs],dim=3.1)
  inputs<-dimSums(a[,,inputs],dim=3.1)
  SNUpE<-outputs/inputs
  SNUpE[is.na(SNUpE)]=0
  SNUpE[is.nan(SNUpE)]=0
  SNUpE[is.infinite(SNUpE)]=0
  SNUpE[SNUpE<0]=0
  #future
  
  data<-toolNUEscenarios(x=SNUpE,weight=inputs)
  
  weight=data$weight
  out=data$x
  
  return(list(
    x=out,
    weight=weight,
    unit="Share",
    description="Soil nitrogen uptake efficiency",
    isocountries =!cellular))
}



