


calcRiskPremium <- function() {
  
  # read data
  x <- readSource("OECD",subtype = "riskClass")
  
  # convert into percent
  x <- x / 100
  
  # delete dimensions that are same and not in the GAMS-code
  getNames(x) <- NULL
  getYears(x) <- NULL
  
  # use GDP as weight
  w <- calcOutput("GDPppp",aggregate=FALSE)[,2005,"gdp_SSP2"]
 
  return(list(x           = x,
              weight      = w,
              unit        = "dimensionless",
              description = "risk premium that lowers the use of capital imports"))
}