calcDem_material_total<-function(){
  warning("This function is depreciated.")
  out<-calcOutput(type = "Demand",aggregate = F)
  x<-collapseNames(out[,,"mat"],collapsedim = 1)
  weight<-NULL
  return(x=list(x=x,weight=weight,
                unit="Mt DM",
                description="total material demand"
  ))
}
