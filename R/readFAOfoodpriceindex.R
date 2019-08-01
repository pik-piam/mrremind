#' @title readFAOfoodpriceindex
#' @description reads in global food price index from the FAO, downloaded from http://www.fao.org/worldfoodsituation/foodpricesindex/en/
#' @return Magpie object with results on global level.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' readSource("FAOfoodpriceindex",convert=FALSE)
#' }


readFAOfoodpriceindex<-function(){
  a<-read.csv(file="food_price_index_nominal_real.csv",stringsAsFactors=FALSE)
  a<-a[-c(1:3),]
  rownames(a)<-paste0("y",a[,1])
  colnames(a)<-c("year","nominal","real")
  a<-a[,-1]
  a[,c(1)]<-as.numeric(a[,c(1)])
  a[,c(2)]<-as.numeric(a[,c(2)])
  out<-clean_magpie(as.magpie(a))
  return(out)
}