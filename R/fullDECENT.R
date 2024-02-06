#' fullDECENT
#'
#' Function that produces the complete regional data set required for the
#' DECENT model.
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @author Lavinia Baumstark, Lukas Feldhaus
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#' @examples
#' \dontrun{
#' fullDECENT()
#' }
#'
fullDECENT <- function(rev = 0) {

  calcOutput("Labour", years = 2010,             round = 8,  file = "f_lab.cs3r")
  calcOutput("GDP", years = 2010,                round = 8,  file = "f_gdp.cs3r")
  calcOutput("RatioPPP2MER",                     round = 8,  file = "pm_shPPPMER.cs4r")
  calcOutput("Capital",                          round = 6,  file = "p29_capitalQuantity.cs4r")
  calcOutput("EmissionsTe",                      round = 5,  file = "f_emissions.cs4r")

}
