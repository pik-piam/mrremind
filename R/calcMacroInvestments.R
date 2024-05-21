
calcMacroInvestments <- function() {
  
  # read in investment share 
  shInv <- readSource("PWT")[,2005,"csh_i"]
  # read in gdp
  gdp <- calcOutput("GDP",aggregate=FALSE)[,2005,"gdp_SSP2"]
  
  # BS 01-03-2019: updated to use new toolFillWithRegionAvg function
  shInv_new <- toolFillWithRegionAvg(shInv, valueToReplace = 0, weight = gdp)
  
  # # calculate macro investments
  data <- shInv_new * gdp
  
  # convert unit
  data <- data / 1000000
  
  getYears(data)  <- NULL
  getNames(data)  <- NULL
  
  return(list(x           = data,
              weight      = NULL, 
              unit        = "trillion 2005US$", 
              description = "Investments in the macro-economic capital stock at constant 2005 national prices"))
}
