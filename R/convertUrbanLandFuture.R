# not useful for gridded urban


convertUrbanLandFuture<-function(x){
  CountryToCell   <- toolGetMapping("CountryToCellMapping.csv", type = "cell")
  x   <- toolAggregate(x, rel=CountryToCell, from="celliso", to="iso", partrel=TRUE)
  
  return(x)
  
}  