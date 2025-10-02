#' @title calcEmiLULUCFCountryAcc
#' @description historical LULUCF CO2 emissions following country accounting
#' @return Magpie object with historical LULUCF emissions
#' @author Felix Schreyer, Falk Benke

calcEmiLULUCFCountryAcc <- function() {
  # read in UNFCCC CRF emissions data
  unfccc <- readSource("UNFCCC")
  out <- collapseNames(unfccc[, , "4_ Total LULUCF|CO2"]) / 1000
  out[is.na(out)] <- 0

  return(list(
    x = out,
    weight = NULL,
    unit = "Mt CO2/yr",
    description = glue::glue("Historical LULUCF CO2 emissions data following \\
                             country accounting taken from UNFCCC database")
  ))
}
