
calcFloorspace <- function() {
  
  data <- readSource("EDGE", subtype = "Floorspace") 
  data <- collapseNames(data[,,"buildings"])
  
  return(list(x=data,weight=NULL, unit = "million m2", description = "Buildings floorspace"))
}
