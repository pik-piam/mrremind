calcSOMinitialsiationPools<-function(){
  past<-findset("past")
  som <- calcOutput("SOM", aggregate=FALSE)
  som <- collapseNames(som[,past,c("soilc")])
  
  return(list(
    x=som,
    weight=NULL,
    unit="Mt C",
    description="Soil carbon in cropland and non-cropland soils.",
    isocountries=FALSE,
    min=0,
    max=1000
    ))
}