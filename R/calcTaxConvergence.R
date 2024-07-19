#' @title calc Tax Convergence
#' @description tax convergence levels for specific regions
#'
#' @return magpie object of the tax convergence levels
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' calcOutput("TaxConvergence")
#' }
#'
calcTaxConvergence <- function() {

  # Read tax convergence levels at specific year and final energy type
  taxConvergence <- readSource("REMIND_11Regi", subtype = "taxConvergence")

  # average weight
  w <- taxConvergence
  w[, , ] <- 1

  # convert data from $2005 to $2017
  taxConvergence <- GDPuc::convertGDP(
    gdp = taxConvergence,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2017 Int$PPP",
    replace_NAs = "with_USA"
  )

  return(list(
    x = taxConvergence,
    weight = w,
    unit = "$/GJ",
    description = "Tax convergence level for specific regions, year and final energy type"
  ))
}
