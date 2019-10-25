


calcCO2Prices <- function() {
  
  # read data
  x <- readSource("ExpertGuess",subtype="co2prices")
  getNames(x) <- NULL
  
  # read data used for weight
  ceds <- calcOutput("Emissions",datasource="CEDS2REMIND",aggregate = FALSE)[,getYears(x),"Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"]
  
  return(list(x           = x,
              weight      = ceds,
              unit        = "US$2005/t CO2",
              description = "CO2 prices in 2010 and 2015"))
}