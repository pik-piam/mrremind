#' @title calcNitrogenFixationRateNatural
#' @description calculates fixation rates from natural ecosystems based on evapostranspiration
#' @return List of magpie objects with results on global level, empty weight, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcNitrogenFixationPast}}
#' \code{\link{readHerridge}} 
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("NitrogenFixationRateNatural")
#' }
#' 
#' @importFrom magclass getRegionList<-
#' @importFrom magclass getRegionList

calcNitrogenFixationRateNatural<-function(){
  
  e_rate <- readSource(type = "LPJml_rev21",subtype="evaporation",convert="onlycorrect")
  t_rate <- readSource(type = "LPJml_rev21",subtype="transpiration",convert="onlycorrect")
  et_rate= dimSums(e_rate+t_rate,dim=3)
  start_year="y1965"
  
  land <- calcOutput("LanduseInitialisation",aggregate = FALSE,cellular=TRUE)
  et=et_rate*dimSums(land,dim=3)
  
  # calibration to global total of 58 Tg from Vitousek et al 2013,
  # assuming linear relation to evapotranspiration from Cleveland et al 1999
  bnf=58/dimSums(setYears(et[,start_year,],NULL),dim=c(1,3))*et
  bnf_rate=bnf/dimSums(land,dim=3)
  bnf_rate[is.na(bnf_rate)]=0
  
  # in case we also have ET for pasture, we could also first calibrate with natveg and the apply to ET rates of pastures. however pasture productivtiy very uncertain
  
  return(list(x=bnf_rate,
              weight=dimSums(land,dim=3),
              unit="Mt Nr / Mha",
              description="Nitrogen fixation  freeliving bacteria",
              isocountries=FALSE)
         )
}
