#' fullVALIDATIONREMIND
#'
#' Function that generates the historical regional dataset against which the
#' REMIND model results can be compared.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @author David Klein
#' @seealso
#' \code{\link{fullREMIND}},\code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullVALIDATIONREMIND()
#' }
#'
fullVALIDATIONREMIND <- function(rev = 0) {

  #-------------- historical data ---------------------------------------------------------------------
  
  calcOutput("Historical", round = 5,  file = "historical.mif", aggregate = "region+global+missingH12")
  
  calcOutput("HistoricalGlobal", round = 5,  file = "historical_global.mif", aggregate = "global")
  
  calcOutput("HistoricalRegion", round = 5,  file = "historical_region.mif", aggregate = "region")
  

}
