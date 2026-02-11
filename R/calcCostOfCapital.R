#' Calculate capital costs for energy technologies
#'
#' Provides REMIND data for 25_WACC Module
#'
#' @author Diamantis Koutsandreas
#'
#' @examples
#' \dontrun{
#' calcOutput("ETH_WACC")
#' }
#'


calcCostOfCapital <- function() {
  output <- readSource("ETH_WACC")
  return(
    list(
      x = output, unit = "%",
      description = "Technology WACC markups across REMIND region",
      isocountries = FALSE
    )
  )
}
