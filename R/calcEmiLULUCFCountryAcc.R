#' @title calcEmiLULUCFCountryAcc
#' @description historical LULUCF emissions following country accounting
#' @return Magpie object with historical LULUCF emissions
#' @param subtype Valid subtypes are 'GHG' and 'CO2'
#' @author Felix Schreyer, Falk Benke

calcEmiLULUCFCountryAcc <- function(subtype) {
  # read in UNFCCC CRF emissions data
  unfccc <- readSource("UNFCCC")

  if (subtype == "CO2") {
    # LULUCF CO2 emissions from UNFCCC database from, convert to Mt CO2/yr
    out <- collapseNames(unfccc[, , "4_ Total LULUCF|CO2"]) / 1000
    # replace NA by 0
    out[is.na(out)] <- 0
    unit <- "Mt CO2/yr"
  } else if (subtype == "GHG") {
    out <- unfccc[, , "4_ Total LULUCF|CO2"] / 1000 +
      unfccc[, , "4_ Total LULUCF|CH4"] / 1000 * 28 +
      unfccc[, , "4_ Total LULUCF|N2O"] / 1000 * 265
    out[is.na(out)] <- 0
    unit <- "Mt CO2eq/yr"
  } else {
    stop("Please define a valid subtype for this function.")
  }

  return(list(
    x = out,
    weight = NULL,
    unit = unit,
    description = paste0(
      "Historical LULUCF ", subtype,
      " emissions data following country accounting taken from UNFCCC database"
    )
  ))
}
