calcLivestock_share<-function(){
  out<-calcOutput(type = "Demand",aggregate = F)
  x<-collapseNames(out[,,"ls"],collapsedim = 1)
  weight<-collapseNames(out[,,"dem"],collapsedim = 1) + 10^-10
  return(x=list(x=x,weight=weight,
                unit="Share",
                description="Share of food demand for animal-based products in total caloric demand, including fish"
  ))
}