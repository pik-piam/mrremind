#conversion i.e. harmonization of data to WDI calcPopulation values is not done here, as convert script doesn't allow for cellular objects
#Use calcGridPop instead

convertGridPop<-function(x,subtype){
  CountryToCell   <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  x   <- toolAggregate(x, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)
  
  return(x)
  #check for harmonization
  #agg1   <- toolAggregate(gridpop1, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)
  #round(agg1/1e6-pop[getRegions(gridpop),past,"pop_SSP2"])
  
  }  