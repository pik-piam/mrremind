#' @title calcAtmosphericRedepositionShare
#' @description Calculates share of volatilised nitrogen emissions that is redeposited on different land types.
#' @param cellular cellular or country level
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



calcAtmosphericRedepositionShare<-function(cellular=FALSE,maxshare=0.8,scenario="rcp45"){

  dep<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",scenario=scenario,cellular=cellular,emission=FALSE,aggregate = FALSE)
  emi<-calcOutput("AtmosphericDeposition",datasource="ACCMIP",scenario=scenario,cellular=cellular,emission=TRUE,aggregate = FALSE)
  
  # limit deposition to maxshare of emissions
  scaling=collapseNames(dimSums(dep,dim=3.1)/emi)
  if(any(scaling<maxshare)){
    # dont scale emissions below maxshare
    scaling[scaling<maxshare] <- maxshare
  }
  dep=dep/scaling*maxshare
  
  share=collapseNames(dep/emi)
  share_glo=dimSums(dep,dim=1,na.rm = T)/dimSums(emi,dim=1,na.rm = T)
  
  missing<-where(is.na(share))$true$regions
  if(length(missing)>0){share[missing,,]<-share_glo}
  weight=collapseNames(emi)

  return(list(
    x=share,
    weight=weight,
    unit="Mt Nr, NH3N and NO2N",
    isocountries=!cellular,    
    description="Share of emitted volatilised nitrogen that is redeposited within the same country. Based on the assumption that most of the N is redeposited in the same country."))
}
