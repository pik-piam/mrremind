#' Calculate baseline emissions of waste
#'
#' Provides REMIND data for CO2 parameters to calculate baseline emissions of
#' waste from population and investment.
#'
#' @return REMIND data for CO2 parameters to calculate baseline emissions of
#' waste from population and investment and corresponding weights (population)
#' as a list of two MAgPIE objects
#' @author Lavinia Baumstark
#' @seealso \code{\link{calcOutput}}, \code{\link{readSource}}
#' @examples
#' \dontrun{
#' calcOutput("calcEconometricEmiParameter")
#' }

calcEconometricEmiParameter <- function() {

  # calculate parameter p3
  edgar <- readSource("EDGAR", subtype = "co2") * 12 / 44 * 1e-6
  pop   <- calcOutput("Population", aggregate = FALSE)[, 2005, "pop_SSP2"] / 1000

  p3Country <- dimSums(edgar[, , c("2A1", "2A2")], dim = 3) / pop
  mapping    <- toolGetMapping(type = "regional", name = "regionmappingEconometricEmi.csv", where = "mappingfolder")
  p3         <- toolAggregate(p3Country[unique(mapping$RepCountry), , ], mapping, weight = NULL)
  p3[is.na(p3)] <- 0
  getNames(p3) <- "co2cement_process.p3"

  # calculate parameter p4
  p4 <- readSource("REMIND_11Regi", subtype = "p4")
  getNames(p4) <- "co2cement_process.p4"
  getYears(p4) <- getYears(p3)

  # convert data from $2005 to $2017

  p4 <- GDPuc::convertGDP(
    gdp = p4,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2017 Int$PPP",
    replace_NAs = "with_USA"
  )

  # combine all parameters
  x <- mbind(p3, p4)
  getYears(x) <- NULL

  return(list(x = x,
              weight = pop,
              unit = "p3 - GtC/Cap, p4 - $US2017/Cap",
              description = "CO2 parameters to calculate baseline emissions of waste from population and investment"))
}
