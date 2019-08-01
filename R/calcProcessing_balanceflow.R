
calcProcessing_balanceflow<-function(){
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)
  
  # add cotton fibres, cause its not in the massabalance calculations
  conv<-calcOutput("Processing_conversion_factors",aggregate = F)[,,"ginning"]
  massbalance[,,"production_estimated"][,,"fibres"][,,"dm"]<-collapseNames(conv[,getYears(massbalance),"ginning"][,,"fibres"][,,"cottn_pro"] * massbalance[,,"production"][,,"dm"][,,"cottn_pro"])
  
  ksd<-findset("ksd")
  ksd_noscp<-setdiff(ksd,"scp")
  balanceflow<-dimSums(massbalance[,,"production"][,,"dm"][,,ksd_noscp],dim=c(3.2,3.3))-dimSums(massbalance[,,"production_estimated"][,,"dm"][,,ksd_noscp],dim=c(3.2,3.3))
  balanceflow<-add_columns(balanceflow,addnm = "scp",dim = 3.1)
  balanceflow[,,"scp"]<-0

  out <- toolHoldConstantBeyondEnd(balanceflow)
  #fading out the balanceflow until 2050.
  out <- convergence(origin = out,aim = 0,start_year = "y2010",end_year = "y2050",type = "s")

  return(list(x=out,weight=NULL,unit="t DM",description="Balanceflow to balance out the assumption of uniform conversion factors worldwide but match FAO."))
}