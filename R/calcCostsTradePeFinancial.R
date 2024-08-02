#' Calculate Trade Cost
#'
#' Provides REMIND data for PE trade cost (energy losses on import, export and use).
#'
#' @author Regina Brecha, Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' \dontrun{
#' calcOutput("CostsTradePeFinancial")
#' }
#'
calcCostsTradePeFinancial <- function() {
  data <- readSource("ExpertGuess", subtype = "costsTradePeFinancial")

  data <- GDPuc::convertGDP(
    gdp = data,
    unit_in = "constant 2005 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = c("linear", "with_USA")
  )

  w <- calcOutput("GDP", aggregate = FALSE)[, 2005, "gdp_SSP2"]

  return(list(
    x = data,
    weight = w,
    unit = "T US$2017/TWa",
    description = "PE tradecosts (financial costs on import, export and use)"
  ))
}
