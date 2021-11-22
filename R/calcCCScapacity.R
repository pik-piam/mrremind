
calcCCScapacity <- function(){
  
  # Read capacity factor inputs
  ccs <- readSource("GlobalCCSinstitute")
  
  # convert from MtCO2/yr to GtC/yr
  out <- 1 / (3.666666666667 * 1000) * ccs
  
  return(list(x           = out, 
              weight      = NULL,
              unit        ="% of capacity", 
              description ="Installed capacity availability - capacity factor (fraction of the year that a plant is running)"              
  ))
  
}
