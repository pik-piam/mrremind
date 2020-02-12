calcShareCHP <- function() {
   
  data <- readSource("IEA", subtype="CHPreport")/100
  getNames(data) <-"bscu"
  w <- calcOutput("IO",subtype="output",aggregate=FALSE)[,2010,c("feelb","feeli")]
  w <- dimSums(w,dim=3)
  
  return(list(x=data,weight=w,
              unit="ratio",
              description="share of electricity from chp on total electricity"))
}