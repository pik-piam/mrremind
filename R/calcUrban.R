#' calcUrban
#' 
#' Merges time series of urban shares for the past and present.  See
#' \code{\link{calcUrbanPast}} for past datasets, and
#' \code{\link{calcUrbanFuture}} for future datasets.  The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point.  All data is calibrated either to the "past"
#' or the "future" dataset.  Currently, the SSP (future) and WDI (past) data
#' have some inconsistencies, which leads to unrealistic figures if the one is
#' scaled on the other.
#' 
#' @param UrbanCalib To what should be calibrated? past or future?
#' @param UrbanPast Urban past data source
#' @param UrbanFuture Urban future data source
#' @param naming naming scheme
#' @return Share of urban population
#' @author Antoine Levesque
#' @seealso \code{\link{convertWDI}},\code{\link{calcGDPpppPast}}
calcUrban <- function(UrbanCalib="past", UrbanPast="WDI", UrbanFuture="SSP",naming="indicator_scenario" ) {
  past<-calcOutput("UrbanPast", UrbanPast=UrbanPast, aggregate = FALSE)
  future<-calcOutput("UrbanFuture", UrbanFuture=UrbanFuture, aggregate = FALSE)
  firstyear<-min(getYears(future,as.integer = T))
  
  if (UrbanCalib == "past") {
    tmp<-dimSums(future/setYears(future[,firstyear,],NULL)*setYears(past[,firstyear,],NULL),dim=3.2)
    tmp[is.nan(tmp)]<-0
    if (firstyear>min(getYears(past,as.integer = T))) {                                                 
      years_past<-getYears(past)[which(getYears(past,as.integer = T)<firstyear)]
      tmp2 <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
      combined<-mbind(tmp2,tmp)
    } else {
      combined<-tmp
    }
    datasettype <- UrbanPast
  } else if (UrbanCalib == "future") {
    tmp<-dimSums(past/setYears(past[,firstyear,],NULL)*setYears(future[,firstyear,],NULL),dim=3.2)
    tmp[is.nan(tmp)]<-0
    if (firstyear>min(getYears(past,as.integer = T))) {                                                 
      years_past<-getYears(past)[which(getYears(past,as.integer = T)<firstyear)]
      tmp2 <- setNames(tmp[,years_past,rep(1,ndata(future))],getNames(future))
      combined<-mbind(tmp2,future)
    } else {
      combined<-future
    }
    datasettype <- UrbanFuture
  } else {
    stop("UrbanCalib has to be past or future")
  }
  
  wp <- calcOutput("Population", aggregate = FALSE)
  getNames(wp) <- gsub("(pop_SSP\\d).*","\\1",getNames(wp))
  combined <- combined[getRegions(wp),getYears(wp),]
  
 
  
  combined<-clean_magpie(combined)
  combined[combined[]=="Inf"] <- 0    # LB: preliminary bug fix
  combined<-setNames(combined,getNames(future))
  
  if(naming=="indicator.scenario"){
    getNames(combined)<-sub(pattern = "_",x=getNames(combined),replacement = ".")
    getSets(combined)<-c("region","year","indicator","scenario")
  } else if (naming!="indicator_scenario"){
    stop("unknown naming scheme")
  }
 
  
  return(list(x=combined,weight=wp,unit="share of population",
              description=paste0("Urban data. Datasource for the Past: ",
                                 UrbanPast,
                                 ". Datasource for the Future: ",
                                 UrbanFuture,
                                 ". Calibrated to ",
                                 datasettype)
  )
  )
  
}
