
calcLimitCCS <- function(){
  
  # Read capacity factor inputs
  data <- readSource("REMIND_11Regi",subtype="ccs")
  
  # add dimensions that are the same for all regions but a dimension of the parameter in the GAMS code
  getNames(data) <- "quan"
  data <- add_dimension(data, dim=3.2, add="rlf",nm="1")
  
  return(list(x           = data, 
              weight      = NULL,
              unit        ="GtC", 
              description ="maximum CO2 storage capacity using CCS technology"              
  ))
  
}

