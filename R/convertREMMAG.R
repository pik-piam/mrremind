convertREMMAG <- function(x,subtype) {
  map <- toolMappingFile("regional","regionmappingMAgPIE.csv")
  
  if(subtype=="ghgprices") {
    y <- toolAggregate(x,map)
  } else if(subtype=="biodem") {
    pop <- calcOutput("Population",aggregate=FALSE)
    y <- toolAggregate(x,map,weight=pop[,2010,1])
  } else {
    stop("Unknown subtype ",subtype)
  }
  return(y)
}