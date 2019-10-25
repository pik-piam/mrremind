

calcVintagesTransport <- function(){
 
  x <- readSource("REMIND_11Regi", subtype = "vintage")
  x[is.na(x)] <- 1
  
  w <- calcOutput("IO",subtype="output",aggregate=FALSE)[,2005,getNames(x,dim=2)]
  w <- dimSums(w,dim=c(3.1,3.2))
  
  return(list(x           = x,
              weight      = w,
              unit        = "none",
              description = "vintages, installed capacities"))
}
  
