convertStrefler <- function(x) {
  
    w <- calcOutput("FAOLand",aggregate=FALSE)[,,"6620",pmatch = TRUE][,2005,]
    y <- toolAggregate(x, "regionmappingGEC.csv", weight=w)
    
    return(y)
}
