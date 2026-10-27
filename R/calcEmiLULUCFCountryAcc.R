#' @title calcEmiLULUCFCountryAcc
#' @description Historical LULUCF CO2 emissions data following the national
#' accounting approach. Where available data reported to the UNFCCC is taken.
#' If not available, data estimated by Forsell et al. (2025),
#' https://pure.iiasa.ac.at/id/eprint/20368/1/advpub_24.001.pdf, is taken.
#' Note that Forsell et al. data reports total GHG LULUCF emissions. However, as
#' non-CO2 LULULUCF emissions are typically small we neglect the difference and
#' use them to calculate the offset between book-keeping and national accounting
#' CO2 emissions in REMIND.
#' @return Magpie object with historical LULUCF emissions
#' @author Felix Schreyer, Falk Benke
calcEmiLULUCFCountryAcc <- function() {
  # read in UNFCCC CRF emissions data
  unfccc <- readSource("UNFCCC")
  unfccc <- magclass::collapseNames(unfccc[, , "4_ Total LULUCF|CO2"]) / 1000

  iiasaHist <- readSource("IIASALanduse", subtype = "historical")
  iiasaHist <- magclass::matchDim(iiasaHist, unfccc, dim = 2)

  out <- unfccc
  out[is.na(out)] <- iiasaHist[is.na(out)]
  out[is.na(out)] <- 0
  return(list(
    x = out,
    weight = NULL,
    unit = "Mt CO2/yr for UNFCCC and Mt CO2eq/yr for Forsell et al.",
    description = glue::glue("Historical LULUCF CO2 emissions data following \\
                             country accounting taken from UNFCCC database. Where not \\
                             available, use Forsell et al.")
  ))
}
