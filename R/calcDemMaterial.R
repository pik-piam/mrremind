calcDemMaterial<-function(){
  out<-calcOutput("FAOmassbalance",aggregate = F)
  out<-collapseNames(out[,,"dm"][,,"other_util"])
  return(x=list(x=out,weight=NULL,
                unit="Mt DM",
                description="historical material demand"
  ))
}
