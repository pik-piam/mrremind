#conversion i.e. harmonization of data to WDI calcPopulation values is not done here, as convert script doesn't allow for cellular objects
#Use calcGridPop instead

convertGCF_SeaLevelRise<-function(x){
  CountryToCell   <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  x   <- toolAggregate(x, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)
  
  return(x)
  
}  