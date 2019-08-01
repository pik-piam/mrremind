# maybe rename to loss?
calcLossShare<-function(){
  massbalance<-calcOutput("FAOmassbalance",aggregate=F)

  # connected to domestic supply, as FAOSTATS flossary states that 
  # "Waste is often estimated as a fixed percentage of availability, the latter being defined as production plus imports plus stock withdrawals."
  wasteshr<-dimSums(massbalance[,,"waste"][,,"dm"],dim=c(3.2,3.3))/dimSums(massbalance[,,"domestic_supply"][,,"dm"],dim=c(3.2,3.3))
  weight<-dimSums(massbalance[,,"domestic_supply"][,,"dm"],dim=c(3.2,3.3))
  newproducts=c("begr","betr","scp","wood","woodfuel")
  #assume a waste share of 1 percent for begr, betr and scp 
  wasteshr[,,newproducts]<-0.01
  weight[,,newproducts]<-1
  wasteshr[,,"pasture"]<-0
  wasteshr[is.na(wasteshr)]<-0
  weight[wasteshr==Inf]<-0
  wasteshr[wasteshr==Inf]<-0
  
  wasteshr <- toolHoldConstantBeyondEnd(wasteshr)
  weight <- toolHoldConstantBeyondEnd(weight)
  
  
  return(list(x=wasteshr,weight=weight,unit="DM share, weight: domestic supply",description="Share of domestic supply wasted"))
}


