correctStockChangeFactors<-function(x){
  
  #rename old "AFR.1"-style in new "GLO.1"-style
  getCells(x) <- paste0("GLO",substring(getCells(x),4))
  
  return(x)
}  