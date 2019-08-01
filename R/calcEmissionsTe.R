
calcEmissionsTe <- function() {
  
  x <- calcOutput("Emissions",datasource="CDIAC",aggregate=FALSE)[,2010,]
  x <- x[,,"Emissions|CO2|Fossil Fuels and Industry (Mt/yr)"] - x[,,"Emissions|CO2|Industrial Processes|Cement (Mt/yr)"] + x[,,"Emissions|CO2|Energy|Bunkers (Mt/yr)"]
  
  # convert from Mt CO2 into GtC
  x <- x / 44 * 12 / 1000
  
  # reduce dimension
  x <- collapseNames(x)
  
  return(list(x=x,weight=NULL,unit="GtC",
              description="CO2 emissions (Fossil Fuels and Industry) from CDIAC"))
}
