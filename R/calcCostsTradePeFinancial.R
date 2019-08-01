#' Calculate Trade Cost
#' 
#' Provides REMIND data for PE trade cost (energy losses on import, export and
#' use).
#' 
#' 
#' @author Regina Brecha, Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("CostsTradePeFinancial")
#' }
#' 
calcCostsTradePeFinancial <- function() {
   
  data <- readSource("ExpertGuess", subtype="costsTradePeFinancial")
  w    <- calcOutput("GDPppp",aggregate=FALSE)[,2005,"gdp_SSP2"]
  
  return(list(x=data,weight=w,
              unit="",
              description="PE tradecosts (financial costs on import, export and use)"))
}
