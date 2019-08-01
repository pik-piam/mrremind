#' @importFrom magclass setNames getNames

calcProcessing_shares<-function(){
  massbalance<-calcOutput("FAOmassbalance",aggregate = F)

  ksd<-findset("ksd")
  kpr<-findset("kpr")
  
  kprocessing<-findset("processing20")
  mb_reduced<-dimSums(massbalance[,,"dm"],dim=c(3.3))
  kmb<-paste("X",kpr,sep="")
  
  production_estimated<-dimSums(mb_reduced[,,"production_estimated"][,,ksd],dim=c(3.2))
  convmatrix<-add_dimension(x = production_estimated,dim = 3.2,add = "primary",nm = kmb)

  convmatrix[,,]<-0
  

  out<-list(x=convmatrix,weight=convmatrix)
  
  calcshare<-function(from,to){
    tmp <- dimSums(mb_reduced[,,from][,,kpr],dim = c(3.2))
    tmpweight<-dimSums(tmp,dim=3.1)
    tmp<-tmp/tmpweight
    out$x[,,to]<-setNames(tmp,paste0("X",getNames(tmp)))     
    out$weight[,,to]<-setNames(dimSums(tmpweight,dim=3.1),paste0("X",getNames(dimSums(tmpweight,dim=3.1)))) 
    return(out)
  }
  out<-calcshare(c("alcohol1","alcohol2","alcohol3"),c("alcohol"))
  out<-calcshare(c("brewers_grain1","distillers_grain1"),c("distillers_grain"))
  out<-calcshare(c("brans1"),c("brans"))
  out<-calcshare(c("branoil1","oil1","oil2"),c("oils"))
  out<-calcshare(c("ethanol1"),c("ethanol"))
  out<-calcshare(c("molasses1"),c("molasses"))
  out<-calcshare(c("sugar1"),c("sugar"))
  out<-calcshare(c("oilcakes1"),c("oilcakes"))
  
  out$x[,,"scp"][,,"Xbegr"] <- 0.5
  out$x[,,"scp"][,,"Xsugr_cane"] <- 0.5
  out$x[,,"scp"][,,"Xfoddr"] <- 0
  out$x[,,"fibres"][,,"Xcottn_pro"] <- 1
  out$weight[,,"scp"][,,"Xbegr"] <-1
  out$weight[,,"scp"][,,"Xsugr_cane"] <- 1
  out$weight[,,"scp"][,,"Xfoddr"] <- 1
  out$weight[,,"fibres"][,,"Xcottn_pro"]<-1
  
  out$x[is.na(out$x)]<-0
  out$weight[is.na(out$weight)]<-0
  
  getNames(out$x, dim=2)<-substring(getNames(out$x, dim=2),2)
  getNames(out$weight, dim=2)<-substring(getNames(out$weight, dim=2),2)
  
  #add years beyond 2010
  out$x <- toolHoldConstantBeyondEnd(out$x)
  out$weight <- toolHoldConstantBeyondEnd(out$weight)
  
  return(list(x=out$x,weight=out$weight,unit="share",description="Share of processed products coming from different primary products"))
}

