


calcIntertempElastSubst <- function() {
  
  # read data
  x <- readSource("ExpertGuess",subtype="ies")
  getNames(x) <- NULL
  getYears(x) <- NULL
  
  # read data used for weight
  pop <- calcOutput("Population",aggregate=FALSE)[,2005,"pop_SSP2"]
  gdp <- calcOutput("GDPppp",aggregate=FALSE)[,2005,"gdp_SSP2"]
  # use half pop and half gdp 
  w <- (pop / dimSums(pop,dim=1)) + setNames(gdp / dimSums(gdp,dim=1),NULL)
 
  
  return(list(x           = x,
              weight      = w,
              unit        = "dimensionless",
              description = "intertemporal elasticity of substitution"))
}