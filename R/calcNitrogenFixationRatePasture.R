calcNitrogenFixationRatePasture<-function(){
  a<-collapseNames(calcOutput("NitrogenBNF",aggregate = FALSE)[,,"past"])
  b<-collapseNames(calcOutput("LanduseInitialisation",aggregate = FALSE)[,,"past"])
  a<-toolHoldConstantBeyondEnd(a)
  b<-toolHoldConstantBeyondEnd(b)
  out<-a/b
  out[is.na(out)]<-0
  
  
  return(list(x=out,
              weight=b,
              unit="t Nr / ha",
              min=0,
              max=0.05,
              description="Biological nitrogen fixation on pastures"))
}
