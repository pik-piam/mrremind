
calcRatioPPP2MER <- function(RatioPPP2MER="SSP") {
  type <- RatioPPP2MER
  if(type=="SSP"){
    data <- readSource("SSP",subtype="ratioPM")
    getNames(data) <- NULL
    getYears(data) <- NULL
  } else if (type=="OECD"){
    data <- readSource("OECD",subtype="ratioPM")
  }  
  else{
    stop(type, " is not a valid source type for population")
  }
  w     <- calcOutput("GDPppp",aggregate=FALSE) 
  weight <- w[,2005,"gdp_SSP2"] #automated?
  return(list(x=data,weight=weight,unit="share",description=paste0("PPP to MER ratio based on ",type)))
}
