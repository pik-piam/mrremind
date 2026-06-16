#' Calculate capital costs for REMIND regions
#'
#' Provides REMIND data for 25_WACC Module
#'
#' @author Diamantis Koutsandreas

calcMacroWACC <- function() {
  output <- readSource("ETH_COUN")
  return(
    list(
      x = output, unit = "%",
      description = "Macroeconomic WACC markups across REMIND regions",
      isocountries = FALSE
    )
  )
}
