#' calcPopulationPast
#' 
#' Calculates a time series of Population. Different sources are available and
#' can be selected in the moinput config(getConfig()$calc$PopulationPast):
#' \itemize{ 
#'   \item \code{WDI}: Source: Worldbank. Taiwan was estimated as the
#'         difference between all countries and the global total.
#'   \item \code{UN_PopDiv}: UN Population Division data. Taiwan is estimated
#'         from "other, non-specified areas". Missing countries have their 
#'         values set to zero.
#' }
#' 
#' @param PopulationPast population past data source
#' @return Population in millions.
#' @author Lavinia Baumstark, Benjamin Bodirsky
#' @seealso \code{\link{convertWDI}},\code{\link{calcGDPpppPast}}
#' @importFrom magclass getNames clean_magpie


calcPopulationPast <- function(PopulationPast="WDI_completed") {
  type <- PopulationPast
  if (type=="DemandModel"){
    data <- collapseNames(readSource("DemandModel")[,,"pop"])
    getNames(data) <- paste0("pop_",getNames(data))
  } else if (type=="WDI") {
    data <- readSource(type = "WDI",subtype = "SP.POP.TOTL",convert = T)
    getNames(data) <- "population"
    
  } else if ('UN_PopDiv' == type) {
    data <- readSource("UN_PopDiv")
    getNames(data) <- 'population'
    
  } else if (type == "WDI_completed"){
    data <- calcOutput("PopulationPast", PopulationPast = "WDI", aggregate = F)
    missing<-where(data==0)$true$region
    
    fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    fill <- time_interpolate(fill,interpolated_year = getYears(data),extrapolation_type = "constant")
    
    missing<-where(setYears(dimSums(data,dim=2),"y2000")==0)$true$region
    data[missing,,]<-fill[missing,,]
    missing<-where(data==0)$true$region
    for(ii in missing){
      missingyears=where(data[ii,,]==0)$true$years
      data[ii,missingyears,] <- time_interpolate(dataset = data[ii,,][,missingyears,,invert=T],interpolated_year = missingyears,extrapolation_type = "constant")
    }
    
    getNames(data) <- "population"
  }
  
    else {
    stop(type, " is not a valid source type for population")
  }
  data<-clean_magpie(data)
  return(list(x=data,weight=NULL,unit="million",description=paste0("Population data based on ",type)))
}
