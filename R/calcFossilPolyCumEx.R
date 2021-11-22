

calcFossilPolyCumEx <- function(){
 
  x <- readSource("REMIND_11Regi", subtype = "ffPolyCumEx")

  return(list(x           = x,
              weight      = NULL,
              unit        = "none",
              description = "vintages, installed capacities"))
}
  
