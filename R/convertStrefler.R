convertStrefler <- function(x) {
  
    w <- calcOutput("FAOLand",aggregate=FALSE)[,2005,"6620|Arable land and Permanent crops.area"]
    y <- toolAggregate(x, "regionmappingGEC.csv", weight=w)
    
    return(y)
}