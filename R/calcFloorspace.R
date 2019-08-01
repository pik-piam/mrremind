
calcFloorspace <- function() {
  
  data <- readSource("EDGE", subtype = "Floorspace") 
  
  
  return(list(x=data,weight=NULL, unit = "million m2", description = "Buildings floorspace"))
}
