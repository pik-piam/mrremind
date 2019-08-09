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
  dimnames(a)[[3]]<-paste0("y",as.numeric(substring(dimnames(a)[[3]],2))+1900)
  a<-as.magpie(a)
  return(a)
}  


