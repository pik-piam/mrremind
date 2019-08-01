#' @importFrom magclass setNames


calcLivestock_kcal_structure<-function(reduce_ruminants=0.5){
  kap<-findset("kap")
  years<-findset("time")
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)
  out<-collapseNames(massbalance[,,"households"][,,"ge"][,,kap],collapsedim=c(2,3))
  lastyear=paste0("y",max(getYears(massbalance,as.integer = T)))
  missingyears<-setdiff(years,getYears(massbalance))
  out<-add_columns(x = out,addnm = missingyears,dim = 2.1)
  out[,missingyears,]<-setYears(out[,lastyear,],NULL)
  aim<-out
  aim[,,"livst_chick"]=out[,,"livst_chick"]+setNames(out[,,"livst_rum"],NULL)*(1-reduce_ruminants)
  aim[,,"livst_rum"]=setNames(out[,,"livst_rum"],NULL)*(reduce_ruminants)
  out<-convergence(origin = out,aim = aim,start_year = "y2010",end_year = "y2050",type = "s")
  weight<-out
  weight[,,]<-setNames(dimSums(out,dim=3),NULL)
  out<-out/dimSums(out,dim=3)
  
  a<-unique(dimnames(which(is.na(out),arr.ind=TRUE))[[1]])
  vcat(verbosity=2,paste0("no values for ",paste(a,collapse = " "),". Set to zero."))
  out[is.nan(out)]<-0
  return(list(x=out,weight=weight,unit="Share",description="Composition of livestock demand. Fixed after historical period to latest year."))
}