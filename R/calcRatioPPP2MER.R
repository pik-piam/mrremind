#' Get PPP-to-MER conversion factors.
#'
#' calcRatioPPP2MER returns conversion factors from the World Bank's WDI to convert monetary values (in constant units
#' with base year equal to the `year` argument) from PPP to MER. So for example from 'constant 2017 Int$PPP' to
#' 'constant 2017 US$MER'.
#'
#' Missing conversion factors are set to 1 and regional aggregation is weighed by GDP from WDI-MI-James.
#'
#'
#' @param year An integer specifying the base year of conversion factor. Defaults to the base year of
#' [mrdrivers::toolGetUnitDollar()], currently: `r mrdrivers::toolGetUnitDollar(returnOnlyBase = TRUE)`.
#' @inherit madrat::calcOutput return
#' @seealso [madrat::calcOutput()]
#' @examples \dontrun{
#' calcOutput("RatioPPP2MER")
#' }
#' @export
calcRatioPPP2MER <- function(year = as.numeric(mrdrivers::toolGetUnitDollar(returnOnlyBase = TRUE))) {

  data <- readSource("WDI", "PA.NUS.PPPC.RF")[, year, ]
  # Replace 0s with 1s. This was done previously. Other solutions here should be taken into consideration.
  data[data == 0] <- 1
  # Set names and years to NULL, for GAMS interface to work.
  getNames(data) <- NULL
  getYears(data) <- NULL

  weight <- calcOutput("GDPPast", pastData = "WDI-MI-James", extension1960 = "none", aggregate = FALSE, years = year)

  list(x = data,
       weight = weight,
       unit = glue::glue("constant {year} US$MER / constant {year} Int$PPP"),
       description = glue::glue("Ratio of GDP in constant {year} US$MER over GDP in constant {year} Int$PPP (source: \\
                                WDI). Can be used to convert between GDP at constant {year} Int$PPP and GDP at \\
                                constant {year} US$MER."))
}
