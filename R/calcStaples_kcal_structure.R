calcStaples_kcal_structure<-function(){
  kst<-findset("kst")
  years<-findset("time")
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)
  out<-collapseNames(massbalance[,,"households"][,,"ge"],collapsedim=c(2,3))
  out<-out[,,kst]

  lastyear=paste0("y",max(getYears(massbalance,as.integer = T)))
  missingyears<-setdiff(years,getYears(massbalance))
  out<-add_columns(x = out,addnm = missingyears,dim = 2.1)
  out[,missingyears,]<-setYears(out[,lastyear,],NULL)
  weight<-out
  weight[,,]<-setNames(dimSums(out,dim=3),NULL)
  out<-out/dimSums(out,dim=3)
  
  a<-unique(dimnames(which(is.na(out),arr.ind=TRUE))[[1]])
  vcat(verbosity=2,paste0("no values for ",paste(a,collapse = " "),". Set to zero."))
  out[is.nan(out)]<-0
  return(list(x=out,weight=weight,unit="Share",description="Composition of staples demand. Fixed after historical period to latest year."))
}