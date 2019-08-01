calcDem_material_structure<-function(){
  warning("This function is depreciated.")
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)
  kall<-findset("kall")
  massbalance<-massbalance[,,kall]
  
  out <- dimSums(massbalance[,,"other_util"][,,c("dm")],dim=c(3.2,3.3),na.rm=T)
  share <- out/dimSums(out[,,kall],dim=3)
  # hold constant after 2010
  share[is.na(share)]<-0
  
  vcat(1,"Crop residues in material demand should fade out. Change function calcDem_material_structure")
  share <- toolHoldConstantBeyondEnd(share)
  weight <- toolHoldConstantBeyondEnd(dimSums(out,dim=3))
  return(list(x=share,weight=weight,unit="Share",description="Composition of material demand. Fixed after historical period to latest year."))
}