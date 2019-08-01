calcResCombustEff<-function(){
  x<-readSource("IPCC","rescombusteff",convert = FALSE)
  x<-setYears(x,NULL)
  return(list(
    x=x,
    weight=NULL,
    unit="share",
    description="Share of nitrogen in biomass that is combusted when crop residues are set on fire."))
}

