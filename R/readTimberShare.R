#' Read Share of timber predicted to come from plantations based on FAO Brown study
#' 
#' @param subtype Data subtype available is abare and brown
#' @return magpie object of the proportion of production coming from plantations
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("TimberShare")
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom magclass magpiesort
#' @importFrom madrat toolSubtypeSelect
#' 
readTimberShare<-function(subtype="abare"){
  x<-read.csv("WorkingPaperFP13_BrownC_March2001.csv")
  keep <- c(1,grep(colnames(x),pattern = subtype))
  x<-x[,keep]
  
  x<-as.magpie(x,spatial=1,temporal=0,datacol=2)
  getYears(x) <- "y2000"
 
  return(x)
}

