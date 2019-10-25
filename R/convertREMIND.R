convertREMIND <- function(x,subtype) {
  
  # remove global dimension
  x <- x["GLO",,,invert=TRUE]
  
  map <- toolMappingFile("regional","regionmappingH12.csv")
  
  if(subtype=="intensive") {
    # No weight for disaggregation because it's prices
    y <- toolAggregate(x,map) 
    
  } else if(subtype=="extensive") {
    # Use population of 2010 as weight for disaggregation
    pop <- calcOutput("Population",aggregate=FALSE)
    y <- toolAggregate(x,map,weight=pop[,2010,1])
    
  } else {
    stop("Unknown subtype ",subtype)
  }
  
  return(y)
}