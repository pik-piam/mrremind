#' Calculate Trade Cost
#'
#' Provides REMIND data for PE trade cost (energy losses on import, export and use).
#'
#' @author Regina Brecha, Lavinia Baumstark
#'
#' @examples
#' \dontrun{
#' calcOutput("CostsTradePeFinancial")
#' }
#'
calcCostsTradePeFinancial <- function() {
  data <- readSource("ExpertGuess", subtype = "costsTradePeFinancial")

  data <- GDPuc::toolConvertGDP(gdp = data,
                                unit_in = "constant 2005 US$MER",
                                unit_out = mrdrivers::toolGetUnitDollar(),
                                replace_NAs = "with_USA")

  w <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)[, 2005, ]

  list(x = data,
       weight = w,
       unit = glue::glue("trillion US${mrdrivers::toolGetUnitDollar(returnOnlyBase = TRUE)}/TWa"),
       description = "PE tradecosts (financial costs on import, export and use)")
}
