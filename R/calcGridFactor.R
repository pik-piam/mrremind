
calcGridFactor <- function() {
  
  x <- readSource("REMIND_11Regi",subtype = "gridFactor")
  getNames(x) <- NULL
  
  w <- dimSums(calcOutput("IO",subtype="output",aggregate=FALSE)[,2005,c("feeli","feelb")],dim=3)
  
  return(list(x           = x,
              weight      = w,
              unit        = "factor", 
              description = "multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions"))
}
