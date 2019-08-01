#' @title calc Early Retirement Adjustment Factor
#' @description provides the extra retirement rate to account for relatively old fleet technologies retirement
#' @return magpie object of additional adjusment percentage to be added to the fraction of the early retired capital in countries to account for relatively old technologies fleet  
#' @author Renato Rodrigues
#' @seealso \code{\link{calcOutput}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput(type="EarlyRetirementAdjFactor")
#' }
#'  

calcEarlyRetirementAdjFactor <- function(){
  
  #loading early retirement adjustment factor data
  data <- readSource("REMIND_11Regi", subtype="earlyRetirementAdjFactor")
  
  #loading weight factor
  IO <- calcOutput("IO",subtype="input",aggregate=FALSE)
  weight <- NULL
  weight <- dimSums(IO[,2005,getNames(data)],dim=3)
  weight <- setNames(weight,getNames(data))
  
  return(list(x=data, weight=weight,
               unit="percentage", 
               description="extra retirement rate for technologies in countries with relatively old fleet",
              mixed_aggregation=TRUE              
   )) 
}