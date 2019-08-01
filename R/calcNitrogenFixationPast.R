#' @title calcNitrogenFixationPast
#' @description calculates fixation from freeliving bacteria and from nitrogen-fixing crops
#' @param fixation_types either "fixation_crops", "fixation_freeliving", or "both"
#' @param sum_plantparts if false, crop residues, belowground residues and harvested organ are reported seperately
#' @param cellular cellular estimates optional
#' @param irrigation if TRUE, distinguishes irrigated and non-irrigated crops
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenFixationPast}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("calcNitrogenFixationPast")
#' }
#' 

calcNitrogenFixationPast<-function(fixation_types="both",sum_plantparts=TRUE,cellular=FALSE,irrigation=FALSE){
  fix_biomass<-NULL
  fix_freeliving<-NULL
  kcr<-findset("kcr")
  past<-findset("past")
  if((fixation_types=="both")&(sum_plantparts==FALSE)){
    warning("sum_plantparts set to true")
    sum_plantparts<-TRUE
  }
  if (fixation_types%in%c("both","fixation_crops")){
    harvest<-collapseNames(calcOutput("Production",products="kcr",cellular=cellular,attributes="nr",irrigation=irrigation,aggregate = FALSE))
    harvest<-add_dimension(harvest,dim = 3.1,add = "data1",nm = "organ")
    res_ag<- collapseNames(calcOutput("ResBiomass",cellular=cellular,plantparts="ag",irrigation=irrigation,attributes="nr",aggregate=FALSE),collapsedim = "attributes")
    res_bg<- collapseNames(calcOutput("ResBiomass",cellular=cellular,plantparts="bg",irrigation=irrigation,attributes="nr",aggregate=FALSE),collapsedim = "attributes")
    biomass<-mbind(harvest,res_ag,res_bg)
    if(sum_plantparts==TRUE){
      biomass<-dimSums(biomass,dim=3.1)
    }
    ndfa<-setYears(readSource("Herridge",subtype = "ndfa"),NULL)
    ndfa<-ndfa[getRegions(biomass),,]
    biomass<-biomass*ndfa
    fix_biomass<-add_dimension(biomass,dim = 3.1,nm = "fixation_crops")
  }
  if (fixation_types%in%c("both","fixation_freeliving")){
    area <- collapseNames(calcOutput("Croparea",cellular=cellular, aggregate=F, sectoral="kcr",physical=TRUE,irrigation=irrigation))
    freeliving <- setYears(readSource("Herridge",subtype = "freeliving",convert=FALSE),NULL)
    freeliving<-area[,past,]*freeliving
    fix_freeliving<-add_dimension(freeliving,dim = 3.1,nm = "fixation_freeliving")
  }

  out<-collapseNames(mbind(fix_biomass,fix_freeliving))
  
  return(list(x=out,
              weight=NULL,
              unit="Mt Nr",
              description="Nitrogen fixation by crops and freeliving bacteria",
              isocountries =!cellular
              ))
}
