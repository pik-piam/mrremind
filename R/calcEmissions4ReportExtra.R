#' Calculates historical emissions needed for estimating emission factors
#' in extra emissions reporting
#
#' @param sectors "CEDS" or "IAMC" to select the sectoral aggregation of the output
#'
#' @return list of magclass with CEDS CH4 and N2O emissionsdata for
#' CEDS sectors for 2020, in Mt N or Mt CH4 per year
#' @author Gabriel Abrahao
calcEmissions4ReportExtra <- function(sectors = "CEDS") {
  incedsall <- readSource("CEDS2025")
  # Keep just the gases we're currently interested in, but also keep all sectors
  out <- incedsall[, 2020, c("ch4", "n2o_n")]

  if (sectors == "IAMC") {
    # Mapping between CEDS sectors and some IAMC sectors we want to estimate emission factors for
    iamcsectormap <- toolGetMapping("mappingCEDS2025toREMIND.csv", type = "sectoral", where = "mrremind")
    # Discard nonmapped sectors and aggregate them
    out <- out[, , intersect(getItems(out, dim = 3.1), iamcsectormap$CEDS2025)]
    out <- toolAggregate(out, rel = iamcsectormap, dim = 3.1, from = "CEDS2025", to = "IAMC")
  }

  return(list(
    x = out, weight = NULL, unit = "Mt N or Mt CH4",
    description = paste0("CEDS sectoral emissions in 2020 for", sectors, " sectors, used for extra reporting")
  ))
}
