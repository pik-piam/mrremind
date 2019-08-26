#' @title calcEmisNitrogenPreagriculture
#' @description 
#' Calculates nitrogenous emissions Nitrogen emissions from soils under 100% natural cover (even for crop and urban) assuming a pre-agricultural time.
#'
#' @param cellular cellular or country outputs
#' @param deposition if TRUE, losses include atmospheric deposition inputs that are lost afterwards. If false, only biological fixation is considered.
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcEmisNitrogenPast}},
#' \code{\link{calcExcretion}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EmisNitrogenPreagriculture")
#' }
#' 


calcEmisNitrogenPreagriculture<-function(cellular=FALSE, deposition=TRUE){
  
  # calibrating the natural rate of leaching ####
  fixnat<-calcOutput("NitrogenFixationRateNatural",aggregate = FALSE)
  land<-calcOutput("LanduseInitialisation",aggregate = FALSE,cellular=TRUE)
  fix<-fixnat*land
  inputs<-(58+6+2.9+1.6+1.6+4)
  
  surplus=fix/58*inputs
  # total natural inputs: 
  # 58 fixation, (Vitousek 2013)
  # estimating global deposition
  # recylcing flows via air, assuming full recycling,disregarding ocean-land interaction
  # assuming proportional deposition to N fixation (not so unrealistic as most deposition should be close to emission)
  # 6 soil and veg Nh3, (Galloway 2004)
  # 2.9 NOX from natural soils (Galloway 2004)
  # 1.6+1.6 fire NOX+Nh3, (Galloway 2004)
  # 4 lightning, (Vitousek 2013)
  # assuming inputs=surplus given constant vegetation
  
  # leaching ####
  #35 Tg of aquatic losses accoding to Vitousek 2013
  # check fire! it seems to be really big, see Braakhekke et al 2017
  #scale factors accordingly
  
  # avoiding division by zero
  surplus[surplus<10^-10]<-10^-10
  
  frac_leach<-calcOutput("IPCCfracLeach",aggregate = FALSE,cellular=TRUE)
  leaching_multiplicationfactor=setYears(35/dimSums(surplus*frac_leach,dim=c(1,3))[,"y1965",],NULL)
  no3<-surplus*frac_leach*leaching_multiplicationfactor
  
  # accumulation in deserts
  
  deserts=(frac_leach==0)
  accumulation_deserts = surplus*deserts
  surplus_nondeserts=surplus-accumulation_deserts
  inputs_nondesert=inputs*(1-deserts)
  inputs_nondesert[inputs_nondesert==0]<-10^-10

  # gaseous losses ####
  
  nox<-(1.6+2.9)/inputs_nondesert*surplus_nondeserts
  nh3<-(6+1.6)/inputs_nondesert*surplus_nondeserts
  # 6.8 Tg from Bouwman, A. F., Fung, I., Matthews, E. & John, J. Global analysis of the potential for N2O production in natural soils. Global Biogeochemical Cycles 7, 557–597 (1993).
  n2o<-(6.8)/inputs_nondesert*surplus_nondeserts  
  
  # n2
    
  n2<-surplus-no3-accumulation_deserts-nox-nh3-n2o

  out<-mbind(
    add_dimension(no3,dim = 3.1,add = "form",nm = "no3_n"),
    add_dimension(nh3,dim = 3.1,add = "form",nm = "nh3_n"),
    add_dimension(nox,dim = 3.1,add = "form",nm = "no2_n"),
    add_dimension(n2o,dim = 3.1,add = "form",nm = "n2o_n_direct"),
    add_dimension(n2,dim = 3.1,add = "form",nm = "n2_n"),
    add_dimension(accumulation_deserts,dim = 3.1,add = "form",nm = "accumulation")
  )
  
  if(deposition==FALSE){
    out<-out/inputs*58 # only fixation
  }
  
  if(cellular==FALSE) {
    mapping<-toolMappingFile(type="cell",name="CountryToCellMapping.csv",readcsv=TRUE)  
    out<-toolAggregate(x=out,rel=mapping,from="celliso",to="iso")
    out<-toolCountryFill(out,fill=colSums(out)*10^-10)
  }
  
  return(list(
    x=out,
    weight=NULL,
    unit="Mt Nr in various forms",
    min=0,
    description="Nitrogen emissions from soils under 100% natural cover (even for crop and urban)"))
}
