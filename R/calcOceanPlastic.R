#' @title calcOceanPlastic
#' @description Calculates amount of ocean plastic based on calibrated waste projections, 
#' for unmanaged waste: all dumps, landfills and dumps from develolping countries, and 2% total as litter
#' Coastal pop from Janbeck et al. 2016 buffer, as constant percentage future pop 
#' @param filtration amount of plastic NOT captured by filtration, janbeck uses 0.15, 0.25, 0.4, but cites a source at .6
#' @author David Chen
#' @return million kg of plastic waste
#' @importFrom magclass dimSums


calcOceanPlastic <- function(filtration=0.4){
 
  #use this for now, but need to do with regressions
  waste <- calcOutput("WasteProj", pc=T, aggregate=F)
  
  #litter 2% of all plastic waste produced (Janbeck 2018)
  litter <- 0.02*waste[,,"plastic"]
  
  waste <- waste[,,c("plastic.landfills","plastic.dumps")]
  
  devIndex <- calcOutput("DevelopmentState", aggregate=F)[,,c("SSP1","SSP2","SSP3","SSP4","SSP5")]
  #Invert development Index, multiply this by landfills to get mismanaged landfill amount
  devIndex <- (devIndex - 1) * -1

  waste[,,"landfills"] <- waste[,,"landfills"] *devIndex
  waste_tot <- dimSums(litter, dim=3.2) + dimSums(waste, dim=3.2)
  
  coastal_pop <- readSource("CoastalPop", convert=T)
  coastal_pop <- coastal_pop[,,c("pop_SSP1","pop_SSP2","pop_SSP3","pop_SSP4","pop_SSP5")]
  getNames(coastal_pop) <- gsub("pop_", "", getNames(coastal_pop))
  coastal_pop <- dimOrder(coastal_pop, c(2,1))

  
  mismanaged <- coastal_pop * waste_tot
  
  OceanPlastic <- filtration*mismanaged
x<- collapseNames(OceanPlastic, 1)
x<-dimOrder(x, c(2,1))
x <- x/1000 #convert from million kg to million tonnes
 
  return(list(
    x=x,
    weight=NULL,
    unit="million tonnes",
    description="plastic waste into ocean",
    isocountries=TRUE
  ))
}
  
  

  