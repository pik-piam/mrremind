#' calcUrbanFuture
#' 
#' Calculates a time series of urban shares, using SSP projections Currently,
#' SSP data does not differentiate between SSPs and has some unconsistencies
#' with WDI in 2010
#' 
#' @param UrbanFuture Urban future data source
#' @return Urban shares
#' @author Antoine Levesque
#' @seealso \code{\link{convertWDI}},\code{\link{calcGDPpppPast}}
calcUrbanFuture <- function(UrbanFuture="SSP") {
  
  type <- UrbanFuture
  if (type == "SSP"){
    data <- collapseNames(readSource("SSP",subtype="all")[,,"Population|Urban|Share"][,,"NCAR"])/100
    getNames(data) <- paste("pop_",gsub("_v[[:alnum:],[:punct:]]*","",getNames(data)),sep="")
    #remove years which only contain 0s as entries
    data <- data[,!apply(data,2,function(x) return(all(x==0))),]
    
    time_inter <- paste0("y",seq(2015,2095,by = 10))
    data <- time_interpolate(data,time_inter,integrate_interpolated_years=TRUE)
    
    time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
    data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
    
    wp <- calcOutput("PopulationFuture", aggregate = FALSE)
    getNames(wp) <- gsub("(pop_SSP\\d).*","\\1",getNames(wp))
    data <- data[getRegions(wp),getYears(wp),]
    

  }else{
    stop(type, " is not a valid source type for urban population")
  }
    
  data<-clean_magpie(data)
  return(list(x=data,weight=wp,unit="per 1",description=paste0("Urbanisation data based on ", type)))
}
