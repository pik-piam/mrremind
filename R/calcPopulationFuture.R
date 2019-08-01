#' calcPopulationFuture
#' 
#' Calculates a time series of Population. Different sources are available and
#' can be selected in the moinput config (getConfig()$calc$PopulationPast):
#' \itemize{ \item \code{"IIASApop"}: Source: IIASA? Lavinia?  \item
#' \code{"IIASApop"}: Source: Lavinia? }
#' 
#' @param PopulationFuture population future data source
#' @return Population in millions.
#' @author Lavinia Baumstark, Benjamin Bodirsky
#' @seealso \code{\link{convertWDI}},\code{\link{calcGDPpppPast}}
#' @importFrom magclass getNames clean_magpie
#' @importFrom madrat readSource


calcPopulationFuture <- function(PopulationFuture="SSP_completed") {
  type <- PopulationFuture
  if (type=="SRES"){type=c("sres_a1_pop","sres_a2_pop","sres_b1_pop","sres_b2_pop")} 
  if(all(type%in%"IIASApop")){
    data <- readSource(type)/1000000
    time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
    data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
  } else if (all(type%in%"SSP")){
    data <- collapseNames(readSource(type,subtype="all")[,,"Population"][,,"IIASA-WiC POP"])
    getNames(data) <- paste("pop_",gsub("_v[[:alnum:],[:punct:]]*","",getNames(data)),sep="")
    # change name of "SSP4d" to "SSP4
    getNames(data)<-sub("SSP4d","SSP4",getNames(data))
    # remove 2000 and 2005, because these years are not complete
    data <- data[,setdiff(getYears(data),c("y2000","y2005")),]
    #remove years which only contain 0s as entries
    data <- data[,!apply(data,2,function(x) return(all(x==0))),]
    time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
    data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
  } else if (all(type%in%c("sres_a1_pop","sres_a2_pop","sres_b1_pop","sres_b2_pop"))){
    data<-NULL
    for (i in type) {
      data <- mbind(data,readSource(type="SRES",subtype=i))
    }
    getNames(data)<-paste0("pop_",substr(getNames(data),6,7))
    time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
    data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
    data[is.na(data)]<-0
  } else if (type %in% c("SRES_completed","SSP_completed")){
    if (type=="SSP_completed") {
      data <- calcOutput("PopulationFuture", PopulationFuture="SSP",  aggregate = F)  
      fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    } else if (type=="SRES_completed") {
      data <- calcOutput("PopulationFuture", PopulationFuture="SRES",  aggregate = F)
      fill <- calcOutput("PopulationFuture", PopulationFuture="SSP_completed",  aggregate = F)[,,"pop_SSP2"]
    }
    
    missing<-where(data==0)$true$region
    fill <- time_interpolate(fill,interpolated_year = getYears(data),extrapolation_type = "constant")
    
    missing<-where(setYears(dimSums(data,dim=2),"y2000")==0)$true$region
    data[missing,,]<-fill[missing,,]
    missing<-where(data==0)$true$region
    for(ii in missing){
      missingyears=where(data[ii,,]==0)$true$years
      data[ii,missingyears,] <- time_interpolate(dataset = data[ii,,][,missingyears,,invert=T],interpolated_year = missingyears,extrapolation_type = "constant")
    }
    
  } else{
    stop(type, " is not a valid source type for population")
  }
  data<-clean_magpie(data)
  # put in alphabetical order
  data<-data[,,order(getNames(data))]
  return(list(x=data,weight=NULL,unit="million",description=paste0("Population data based on ",type)))
}
