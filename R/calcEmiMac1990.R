#' Calculate baseline emissions for maccs for 1990
#' 
#' Provides REMIND data for baseline emissions for maccs for 1990.
#' 
#' 
#' @return REMIND data for baseline emissions for maccs for 1990 and
#' corresonding weights (NULL) as a list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("calcEmiMac1990")
#' }
#' @importFrom magclass getNames<- getYears<-
calcEmiMac1990 <- function() {
  
  # emissions for the calculation of econometric paramter p1
  ch4       <- readSource("EDGAR",subtype="ch4_history") * 1e-3   
  
  ch4wsts  <- dimSums(ch4[,1990,c("6B")],dim=3)
  getNames(ch4wsts) <- "ch4wsts"
  ch4wstl  <- dimSums(ch4[,1990,c("6A","6C","6D")],dim=3)
  getNames(ch4wstl) <- "ch4wstl"
  
  # combine all parameters
  x <- mbind(ch4wsts,ch4wstl)
  getYears(x) <- NULL 
  return(list(x=x,
              weight=NULL,
              unit="MtCH4",
              description="emissions in 1990",
              note=c('used to calculate econometric emission parameter p1')))
}
