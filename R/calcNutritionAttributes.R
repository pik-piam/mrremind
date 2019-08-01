calcNutritionAttributes<-function(){
  massbalance<-calcOutput("FAOmassbalance",aggregate=F)
  household<-dimSums(massbalance[,,"households"][,,c("ge","nr")],dim=c(1,3.2),na.rm=T)
  
  # milling still includes brans, which have to be considered for massbalance correction calcs
  # therefore we use flour for the correction
  fooduse_flour<-dimSums(massbalance[,,c("food","flour1")][,,c("ge","nr")],dim=c(1,3.2),na.rm=T)
  
  if(any(household>fooduse_flour)) {
    warning(
      paste("The following items violate massbalance constraints:",
            paste(unique(dimnames(household)[[3]][which(household>fooduse_flour,arr.ind = T)[,3]]),collapse=" "),
            ". Violating items are corrected through household balance flow."
      ))  
    household[household>fooduse_flour]<-fooduse_flour[household>fooduse_flour]
  }
  
  # but now we want them as share of the milling quantity
  fooduse_milling<-dimSums(massbalance[,,c("food","milling")][,,c("dm")],dim=c(1,3.2,3.3),na.rm=T)
  out<-household/fooduse_milling
  out[out==Inf]<-0 
  out[is.nan(out)]<-0
  out[,,"ge"]<-out[,,"ge"]/4.184
  out[,,"nr"]<-out[,,"nr"]*6.258
  dimnames(out)[[3]]<-gsub(".nr",dimnames(out)[[3]],replacement = ".protein")
  dimnames(out)[[3]]<-gsub(".ge",dimnames(out)[[3]],replacement = ".kcal")
  
  # add missing products and delete unconsidered ones
  kall<-findset("kall")
  out<-out[,,kall]
  out[,,"scp"][,,"kcal"]<-20.9/4.184
  out[,,"scp"][,,"protein"]<-0.7
  
  #add years beyond 2010
  years<-findset("time")
  lastyear=paste0("y",max(getYears(out,as.integer = T)))
  missingyears<-setdiff(years,getYears(out))
  out<-add_columns(x = out,addnm = missingyears,dim = 2.1)
  out[,missingyears,]<-setYears(out[,lastyear,],NULL)
  return(list(x=out,weight=NULL,unit="mio kcal / t DM, t Protein / t DM",description="Values from FAO Food Supply. Describe final calory and protein supply of a product dedicated for fooduse."))
}