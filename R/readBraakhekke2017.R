#' @title readBraakhekke2017
#' @description reads in LPJguess results for nitrogen runs from the publication 
#' Braakhekke, M. C. et al. Nitrogen leaching from natural ecosystems under global change: a modelling study. Earth Syst. Dynam. 8, 1121–1139 (2017).
#' @param subtype subtype filename without NC
#' @return magpie object 
#' 
#' @seealso
#' \code{\link{readLPJml_rev21}}
#' 
readBraakhekke2017 <- function(subtype="soilN") {
  file = paste0(subtype,".nc")
  a<-read.magpie(file)
  a<-unwrap(a)
  a<-aperm(a,c(1,3,4,2))
  if(dim(a)[[2]]==106){
    b<-a
  } else if (dim(a)[[2]]==106*12){
    b<-a[,1:106,,,drop=FALSE]*0
    dimnames(b)[[2]]<-1901:2006
    for(year in 0:105) {
      b[,year+1,,] <- rowSums(a[,(1:12)+year*12,,],dims=1)
    }
  } else {stop("wrong format")}
  out<-as.magpie(b)
  out<-dimSums(out,dim=3)
  out<-setNames(out,subtype)
  out<-toolCell2isoCell(out)
  return(out)
}  


