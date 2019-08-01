calcVegfruit_share<-function(){
  out<-calcOutput(type = "Demand",aggregate = F)
  x<-collapseNames(out[,,"vegfruit_share"],collapsedim = 1)
  weight<-collapseNames(out[,,"dem"],collapsedim = 1) + 10^-10
  return(x=list(x=x,weight=weight,
                unit="Share",
                description="Share of food demand for vegetable, fruit and nut products in total caloric demand"
  ))
}
