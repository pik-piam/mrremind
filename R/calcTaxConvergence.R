#' @title calc Tax Convergence
#' @description tax convergence levels for specific regions
#'
#' @return magpie object of the tax convergence levels
#' @param subtype either taxConvergence or taxConvergenceRollback
#'
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' calcOutput("TaxConvergence")
#' }
#'
calcTaxConvergence <- function(subtype) {

  if (subtype == "taxConvergence") {

    # Read tax convergence levels at specific year and final energy type
    taxConvergence <- readSource("REMIND_11Regi", subtype = "taxConvergence")

    # convert data from $2005 to $2017
    taxConvergence <- GDPuc::toolConvertGDP(
      gdp = taxConvergence,
      unit_in = "constant 2005 US$MER",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )
  } else {
    taxConvergence <- readSource("ExpertGuess", subtype = "taxConvergenceRollback")
  }

  # average weight
  w <- taxConvergence
  w[, , ] <- 1

  return(list(
    x = taxConvergence,
    weight = w,
    unit = "US$2017/GJ",
    description = "Tax convergence level for specific regions, year and final energy type"
  ))
}
