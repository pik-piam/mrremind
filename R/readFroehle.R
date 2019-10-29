#' Read parameters of Froehle equations
#' 
#' Publication Froehle, Andrew W. 2008 "Climate Variables as Predictors of Basal Metabolic Rate: New Equations" American Journal of Human Biology: The Official Journal of the Human Biology Council 20 (5): 510-29. doi: 10.1002/ajhb.20769.
#' 
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("Froehle") }
#' 

readFroehle<- function() {
  file <- "energy_requirements.csv"
  file2<-read.csv(file, sep=",",dec = ".",header = 2)
  file2<-as.magpie(file2,spatial=0,temporal=0,datacol=3)
  return(file2)
}  
