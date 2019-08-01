#' calcUrbanPast
#' 
#' Calculates a time series of urban shares
#' 
#' @param UrbanPast Urban past data source
#' @return Urban shares in population
#' @author Antoine Levesque
#' @seealso \code{\link{convertWDI}},\code{\link{calcGDPpppPast}}
calcUrbanPast <- function(UrbanPast="WDI") {
  type <- UrbanPast
  if (type == "WDI"){
#     share <- readSource(type = type,subtype = "SP.URB.TOTL.IN.ZS",convert = T)/100
#     pop<-readSource(type,subtype = "SP.POP.TOTL")
#     
#     years<-intersect(getYears(share),getYears(pop))
#     data=share[,years,]*pop[,years,]
    data <- readSource(type = type,subtype = "SP.URB.TOTL.IN.ZS",convert = T)/100
    getNames(data) <- "urbanPop"

    
    }else {
    stop(type, " is not a valid source type for urban shares")
    }
  
  wp <- calcOutput("PopulationPast",PopulationPast="WDI", aggregate = FALSE)
  
  data <- data[getRegions(wp),getYears(wp),]
  
  
  
  data<-clean_magpie(data)
 
  return(list(x=data,weight=wp,unit="per 1",description=paste0("Urbanisation data based on ", type)))
}
