#' calcPopulation
#' 
#' Merges time series of population for the past and present. Different sources
#' are available and can be selected in the moinput config. See
#' \code{\link{calcPopulationPast}} for past datasets, and
#' \code{\link{calcPopulationFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated either to the "past" or
#' the "future" dataset as specified in "getConfig()$calc$PopulationCalib".
#' 
#' @param PopulationCalib to what should be calibrated? past, future or a transition?
#' @param PopulationPast population past data source
#' @param PopulationFuture population future data source
#' @param FiveYearSteps Only five year steps if TRUE, FALSE returns years from source data
#' @param naming naming scheme
#' 
#' @return Population in millions.
#' @author Lavinia Baumstark, Benjamin Bodirsky
#' @seealso \code{\link{convertWDI}},\code{\link{calcGDPpppPast}}
#' @importFrom magclass setNames ndata clean_magpie
#' @importFrom madrat calcOutput


calcPopulation <- function(PopulationCalib="past_transition", PopulationPast="WDI_completed", PopulationFuture="SRES_SSP_completed", FiveYearSteps = TRUE, naming="indicator_scenario") {
  if(PopulationFuture=="SRES_SSP_completed") {
    combined1 <- calcOutput(type = "Population",PopulationCalib=PopulationCalib, PopulationPast=PopulationPast, PopulationFuture="SSP_completed", FiveYearSteps=FiveYearSteps,aggregate=FALSE)
    combined2 <- calcOutput(type = "Population",PopulationCalib=PopulationCalib, PopulationPast=PopulationPast, PopulationFuture="SRES_completed",FiveYearSteps=FiveYearSteps,aggregate=FALSE)
    combined  <- mbind(combined1,combined2)
    datasettype <- PopulationCalib
  } else {
    past   <- calcOutput("PopulationPast",PopulationPast=PopulationPast,aggregate=FALSE)
    future <- calcOutput("PopulationFuture",PopulationFuture=PopulationFuture,aggregate=FALSE)
    firstyear <- min(getYears(future,as.integer = TRUE))
  
    if (PopulationCalib == "past") {
      tmp <- future/setYears(future[,firstyear,],NULL)
      tmp[is.nan(tmp)]<-1
      tmp<- dimSums(tmp*setYears(past[,firstyear,],NULL),dim=3.2)
      if (firstyear>min(getYears(past,as.integer = TRUE))) {                                                 
        years_past <- getYears(past)[which(getYears(past,as.integer = TRUE)<firstyear)]
        tmp2       <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
        combined   <- mbind(tmp2,tmp)
      } else {
        combined <- tmp
      }
      datasettype <- PopulationPast
    } else if (PopulationCalib == "future") {
      tmp <- dimSums(past/setYears(past[,firstyear,],NULL)*setYears(future[,firstyear,],NULL),dim=3.2)
      tmp[is.nan(tmp)]<-0
      if (firstyear>min(getYears(past,as.integer = TRUE))) {                                                 
        years_past <- getYears(past)[which(getYears(past,as.integer = TRUE)<firstyear)]
        tmp2       <- setNames(tmp[,years_past,rep(1,ndata(future))],getNames(future))
        combined   <- mbind(tmp2,future)
      } else {
        combined <- future
      }
      datasettype <- PopulationFuture
    } else if (PopulationCalib == "transition") {
      # end of transisiton, from this time on the future values are used
      yEnd <- 2020
      # last year of past data
      lastyear <- max(getYears(past,as.integer = TRUE))
      # generate past data for all future scenarios
      if (firstyear < lastyear) {                                                 
        years_past  <- getYears(past)[which(getYears(past,as.integer=TRUE)<=firstyear)]
        tmpPast     <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
        years_future  <- getYears(future)[which(getYears(future,as.integer=TRUE)>=lastyear)]
        tmpPast     <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
        years_trans <- getYears(future,as.integer=TRUE)[which(getYears(future,as.integer=TRUE) >= firstyear 
                                            & getYears(future,as.integer=TRUE) <= yEnd)]
        tmpTrans    <- new.magpie(getRegions(future),years_trans,getNames(future),fill=0)
        for(t in years_trans) {
          tmpTrans[,t,] <- (  (max(years_trans) - t)/(max(years_trans) - min(years_trans)) * setYears(tmpPast[,firstyear,],t) 
                            + (t - min(years_trans))/(max(years_trans) - min(years_trans)) * setYears(future[,yEnd,],t)       )
        }  
        combined   <- mbind(tmpPast[,which(getYears(tmpPast,as.integer=TRUE)<firstyear),],
                            tmpTrans,
                            future[,which(getYears(future,as.integer=TRUE)>yEnd),])
      } else {
        stop("the past and future data need to have some overlap")
      }
      datasettype <- paste0("transition between ",PopulationPast, "and ",PopulationFuture," with a transition period until ",yEnd)
    } else if (PopulationCalib == "past_transition") {
      # end of transisiton, from this time on the future values are used
      yEnd <- 2050
      # last year of past data, that also exist in future data
      lastyear <- max( intersect(getYears(past,as.integer = TRUE),getYears(future,as.integer = TRUE)) )
      # generate past data for all future scenarios
      if (firstyear > min(getYears(past,as.integer = TRUE))) {                                                 
        years_past  <- getYears(past)[which(getYears(past,as.integer=TRUE) <= lastyear)]
        tmpPast     <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
        years_trans <- getYears(future,as.integer=TRUE)[which(getYears(future,as.integer=TRUE) >= lastyear 
                                                              & getYears(future,as.integer=TRUE) <= yEnd)]
        diff_in_lastyear <- tmpPast[,lastyear,] - future[,lastyear,]
        tmpTrans    <- new.magpie(getRegions(future),years_trans,getNames(future),fill=0)
        for(t in years_trans) {
          tmpTrans[,t,] <- future[,t,] + setYears(diff_in_lastyear,t) * ( (max(years_trans) - t)/(max(years_trans) - min(years_trans)) ) 
        }  
        combined   <- mbind(tmpPast[,which(getYears(tmpPast,as.integer=TRUE) < lastyear),],
                            tmpTrans,
                            future[,which(getYears(future,as.integer=TRUE) > yEnd),])
      } else {  
        stop("the past and future data need to have some overlap")
      }
      datasettype <- paste0("used past data and afterwards transition between ",PopulationPast, "and ",PopulationFuture," with a transition period until ",yEnd)
    
    } else {
      stop("PopulationCalib has to be past, future, transition or past_transition")
    }
    if (FiveYearSteps){
      years    <- findset("time") 
      combined <- combined[,years,]
    }
    combined <- clean_magpie(combined)
    combined[combined[]=="Inf"] <- 0    # LB: preliminary bug fix
    combined <- setNames(combined,getNames(future))
  }   # close if (PopulationFuture)
  
  if(naming=="indicator.scenario"){
    getNames(combined)<-sub(pattern = "_",x=getNames(combined),replacement = ".")
    getSets(combined)<-c("region","year","indicator","scenario")
  } else if (naming!="indicator_scenario"){
    stop("unknown naming scheme")
  }
  
  # add SSP1plus/SDP scenario as copy of SSP1, might be substituted by real data later
  if(!("pop_SDP" %in% getNames(combined,dim=1))){
     if("pop_SSP1" %in% getNames(combined,dim=1)){ 
        combined_SDP <- combined[,,"pop_SSP1"]
        getNames(combined_SDP) <- gsub("pop_SSP1","pop_SDP",getNames(combined_SDP))
        combined <- mbind(combined,combined_SDP)
     }
  }
       
  return(list(x=combined,weight=NULL,unit="million",
              description=paste0("Population data. Datasource for the Past: ",
                                 PopulationPast,
                                 ". Datasource for the Future: ",
                                 PopulationFuture,
                                 ". Calibrated to ",
                                 datasettype)
              )
  )
         
}
