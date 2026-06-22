#' Calculate the weighted average cost of capital (WACC) markups for REMIND regions.
#' These values are applied to macroeconomic investments.
#'
#'
#' Provides REMIND data for 25_WACC Module
#'
#' @author Diamantis Koutsandreas

calcMacroWACC <- function() {
  output <- readSource("ETH_COUN")
  getNames(output) <- NULL
  return(
    list(
      x = output, unit = "%",
      description = "Macroeconomic WACC markups across REMIND regions",
      isocountries = FALSE
    )
  )
}
