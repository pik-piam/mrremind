#' @importFrom magclass getCells<- getCells
correctUrbanLandFuture<-function(x,subtype){
  
  if(any(is.na(x))){
    vcat(verbosity=1, paste(sum(is.na(x))/length(x)*100,"% of data points with NAs in LUH2. set to 0."))
    x[is.na(x)]<-0
  }
  if(any(x<0)){
    vcat(verbosity=1, paste(sum(x<0)/length(x)*100,"% of data points with negative values in LUH2. set to 0."))
    x[x<0]<-0
  }
  
  #rename old "AFR.1"-style in new "GLO.1"-style
  getCells(x) <- paste0("GLO",substring(getCells(x),4))
  x<-toolCell2isoCell(x)


return(x)
}  