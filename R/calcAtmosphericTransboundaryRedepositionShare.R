#' @title calcAtmosphericRedepositionShare
#' @description Calculates share of volatilised nitrogen emissions that is redeposited on different land types.
#' @param maxshare the maximum amount of emissions deposited within the same cell or country. The remainder will be handled as global emission
#' @param scenario scenario
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("AtmosphericRedepositionShare")
#' }
#' 



calcAtmosphericTransboundaryRedepositionShare<-function(maxshare=0.8,scenario="rcp45"){

  redep_share<-calcOutput("AtmosphericRedepositionShare",scenario=scenario,cellular=FALSE,aggregate = FALSE)
  dep<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",glo_incl_oceans=FALSE,scenario=scenario,cellular=FALSE,emission=FALSE,aggregate = FALSE)
  dep_glo<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",glo_incl_oceans=TRUE,scenario=scenario,cellular=TRUE,emission=FALSE,aggregate = FALSE)
  emi<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",glo_incl_oceans=FALSE,cellular=FALSE,emission=TRUE,aggregate = FALSE)
  emi_glo<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",glo_incl_oceans=TRUE,cellular=TRUE,emission=TRUE,aggregate = FALSE)
  
  if(any(abs(dep_glo-emi_glo)>0.2)){vcat(2,"Mismatch between global emissions and deposition in ACCMIP dataset")}
  
  land<-findset("land")
  local_deposition=emi*redep_share[,,land]
  transboundary_deposition = collapseNames(dep - local_deposition)
  transboundary_emissions = collapseNames(emi_glo - dimSums(local_deposition,dim=c(1,3.4)))

  TransboundaryRedepositionShare = transboundary_deposition / transboundary_emissions
  
  return(list(
    x=TransboundaryRedepositionShare,
    weight=NULL,
    unit="Mt NH3N and NO2N",
    description="Share of emitted volatilised nitrogen that is redeposited within the same country. Based on the assumption that most of the N is redeposited in the same country."))
}
