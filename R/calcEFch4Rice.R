#' @title calcEFch4Rice
#' @description emission factors for methane from flooded rice fields, depending on phyiscal area or area harvested. The emission factors were calculated based on FAOSTAT estimates due to lack of all necessary parameters in the IPCC Guidelines
#' @param physical if true physical area, if false area harvested
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{calcEFch4AWMS}},
#' \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("EFch4Rice")
#' }
#' 
#' @importFrom magclass getSets
#' @importFrom stats median
#' @importFrom grDevices boxplot.stats

calcEFch4Rice<-function(physical=TRUE){
  past<-findset("past")
  emis<-readSource("FAO","EmisAgRiceCult")
  
  ef<-emis[,,"Emissions_(CH4)_(Rice_cultivation)_(Gigagrams)"]/emis[,,"area_harvested"]
  
  # check whether emission factors vary over time
  if(any(round(ef["IND",,],2)!=round(setYears(ef["IND","y2000",],NULL),2))){
    stop("FAOSTAT seems to have changed, recheck this function, ef seems dynamic over time")
  }
  
  weight<-calcOutput("Croparea",sectoral = "kcr",physical=physical,aggregate = FALSE)[,past,]
  weight<-weight[,,"rice_pro"]
  weight<-toolHoldConstantBeyondEnd(weight)
  years<-intersect(getYears(weight), getYears(emis))
  ef<-emis[,years,"Emissions_(CH4)_(Rice_cultivation)_(Gigagrams)"]/weight[,years,]
  
  # select a value without NA
  ef<-as.magpie(apply(ef,MARGIN = 1,FUN = median,na.rm=TRUE))
  #select maximum ef for countries without estimates
  upper_value=boxplot.stats(ef)$stats[4]
  ef[is.na(ef)]=upper_value
  ef[ef==0]=upper_value

  
  if (physical==TRUE){
    unit="t CH4 per ha physical area"
  } else if (physical==FALSE){
    unit="t CH4 per ha area harvested"
  } else { stop("so nicht. physical is boolean!")}
  
  # transform from Gg 10^9g to Mt
  ef=ef/1000
  
  weight<-collapseNames(weight)
  out<-weight
  out[,,]<-ef
  
  return(list(x=out,
              weight=weight,
              unit=unit,
              description="Emission factor per area according to the FAOSTAT emission estimates",
              min=0,
              max=200)
  )                   
}

