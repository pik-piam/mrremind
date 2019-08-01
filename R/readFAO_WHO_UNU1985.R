#' Read parameters of Schofield equations
#' 
#' Food and Agriculture Organization of the United Nations, World Health Organization, and United Nations University. 1985. "Energy and protein requirements." http://www.who.int/iris/handle/10665/39527.
#' 
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource("Schofield") }
#' 

readFAO_WHO_UNU1985<- function() {
  file <- "energy_requirements.csv"
  file2<-read.csv(file, sep=",",dec = ".",header = 2)
  file2<-as.magpie(file2,spatial=0,temporal=0,datacol=3)
  return(file2)
}  
