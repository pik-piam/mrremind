#' calcGDPppp
#' 
#' Merges time series of GDP in Purchase Power Parity (PPP) of International
#' Dollars of the year 2005.  The source is selected in the config file. See
#' \code{\link{calcGDPpppPast}} for past datasets, and
#' \code{\link{calcGDPpppFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated either to the "past" or
#' the "future" dataset as specified in "getConfig()$calc$PopulationCalib".
#' \itemize{ \item \code{WDI}: The PPP estimate from the World Developmnet
#' Indicators (WDI) are supplemented by values for Argentina, Syria and Somalia
#' which are missing in the database. The values were taken from World Bank.
#' 2014. Purchasing Power Parities and the Real Size of World Economies: A
#' Comprehensive Report of the 2011 International Comparison Program. The World
#' Bank. http://elibrary.worldbank.org/doi/book/10.1596/978-1-4648-0329-1.
#' table 2.13 Then, the 2011 estimate is extrapolated with the GDP projection
#' in local currency units (LCU), as these growth rates should be approximately
#' what the growth models predict. The price index from 2011 was transformed
#' into 2005 equivalents using the inflation rate of the United States(US), as
#' the PPPs are in USDollar equvialents. \item \code{PWP}: Penn World Tables }
#' 
#' @param GDPpppCalib to what should be calibrated? past, future or a transition?
#' @param GDPpppPast GDPppp past data source
#' @param GDPpppFuture GDPppp future data source
#' @param FiveYearSteps Only five year steps if TRUE, FALSE returns years from source data
#' @param naming naming scheme
#' 
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @author Lavinia Baumstark, Benjamin Bodirsky
#' @seealso \code{\link{convertWDI}}
#' @importFrom magclass setNames clean_magpie

calcGDPppp <- function(GDPpppCalib="past_transition",
                       GDPpppPast="IHME_USD05_PPP_pc_completed",
                       GDPpppFuture="SRES_SSP_completed",
                       FiveYearSteps = TRUE,
                       naming="indicator_scenario") {

  if(GDPpppFuture=="SRES_SSP_completed") {
    # using the direct function call instead of calcOutput to avoid caching.
    combined1 <- calcOutput(type = "GDPppp",GDPpppCalib=GDPpppCalib, GDPpppPast=GDPpppPast, GDPpppFuture="SSP_completed",FiveYearSteps=FiveYearSteps,aggregate=F)
    combined2 <- calcOutput(type = "GDPppp",GDPpppCalib=GDPpppCalib, GDPpppPast=GDPpppPast, GDPpppFuture="SRES_completed",FiveYearSteps = FiveYearSteps,aggregate=F)
    combined  <- mbind(combined1,combined2)
    datasettype <- GDPpppCalib
  } else {
    past   <- calcOutput("GDPpppPast", GDPpppPast=GDPpppPast, aggregate = FALSE)
    future <- calcOutput("GDPpppFuture", GDPpppFuture=GDPpppFuture, aggregate = FALSE)
    firstyear <- min(getYears(future,as.integer = TRUE))
  
    if (GDPpppCalib == "past") {
      tmp<-dimSums(future/setYears(future[,firstyear,],NULL)*setYears(past[,firstyear,],NULL),dim = 3.2)
      tmp[is.nan(tmp)]<-0
      if (firstyear>min(getYears(past,as.integer = T))) {                                                 
        years_past<-getYears(past)[which(getYears(past,as.integer = T)<firstyear)]
        tmp2 <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
        combined<-mbind(tmp2,tmp)
      } else {
        combined<-tmp
      }
      datasettype <- GDPpppPast
    } else if (GDPpppCalib == "future") {
      tmp<-dimSums(past/setYears(past[,firstyear,],NULL)*setYears(future[,firstyear,],NULL),dim = 3.2)
      tmp[is.nan(tmp)]<-0
      if (firstyear>min(getYears(past,as.integer = T))) {                                                 
        years_past<-getYears(past)[which(getYears(past,as.integer = T)<firstyear)]
        tmp2 <- setNames(tmp[,years_past,rep(1,ndata(future))],getNames(future))
        combined<-mbind(tmp2,future)
      } else {
        combined<-future
      }
      datasettype <- GDPpppFuture
    } else if (GDPpppCalib == "transition") {
      # end of transisiton, from this time on the future values are used
      yEnd <- 2020
      # generate past data for all future scenarios
      if (firstyear>min(getYears(past,as.integer = TRUE))) {                                                 
        years_past  <- getYears(past)[which(getYears(past,as.integer=TRUE)<=firstyear)]
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
      datasettype <- paste0("transition between ",GDPpppPast, "and ",GDPpppFuture," with a transition period until ",yEnd)  
    } else if (GDPpppCalib == "past_transition") {
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
      datasettype <- paste0("used past data and afterwards transition between ",GDPpppPast, "and ",GDPpppFuture," with a transition period until ",yEnd)
    } else {
      stop("GDPpppCalib has to be past, future, transition or past_transition")
    }
    if (FiveYearSteps){
      years<-  findset("time") 
      combined<-combined[,years,]
    }
    combined<-clean_magpie(combined)
    combined[combined[]=="Inf"] <- 0    # LB: preliminary bug fix
    combined<-setNames(combined,getNames(future))
  }  # close if (GDPpppFuture)
  
  if(naming=="indicator.scenario"){
    getNames(combined)<-sub(pattern = "_",x=getNames(combined),replacement = ".")
    getSets(combined)<-c("region","year","indicator","scenario")
  } else if (naming!="indicator_scenario"){
    stop("unknown naming scheme")
  }
  
  # add SSP1plus/SDP scenario as copy of SSP1, might be substituted by real data later
  if(!("gdp_SDP" %in% getNames(combined,dim=1))){
     if("gdp_SSP1" %in% getNames(combined,dim=1)){ 
        combined_SDP <- combined[,,"gdp_SSP1"]
        getNames(combined_SDP) <- gsub("gdp_SSP1","gdp_SDP",getNames(combined_SDP))
        combined <- mbind(combined,combined_SDP)
     }
  }  
       
  return(list(x=combined,weight=NULL,unit="Million US Dollar in currency units of the calibration dataset",
              description=paste0("Million US Dollar in currency units of the calibration dataset. Datasource for the Past: ",
                                 GDPpppPast,
                                 ". Datasource for the Future: ",
                                 GDPpppFuture,
                                 ". Calibrated to ",
                                 datasettype
                                 )))
}
