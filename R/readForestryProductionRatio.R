#' Read Forestry Production Ratio
#' 
#' @return magpie object of the proportion of production coming from plantations
#' @author Abhijeet Mishra
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("ForestryProductionRatio")
#' }
#' 
#' @importFrom magclass as.magpie
#' @importFrom magclass magpiesort
#' @importFrom madrat toolSubtypeSelect

readForestryProductionRatio<-function(){
  x<-read.csv("forestry_production_ratio.csv",header = F)

  x<-as.magpie(x,spatial=1,temporal=2,datacol=3)
  x <- setNames(x,NULL)
  
  return(x)
}

