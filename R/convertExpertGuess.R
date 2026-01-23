#' @title convertExpertGuess
#' @description Converts data from expert guess
#' @param x unconverted magpie object from read-script
#' @inheritParams readExpertGuess
#' @author Falk Benke
#'
convertExpertGuess <- function(x, subtype) {
  if (subtype %in% c(
    "capacityFactorRules",
    "costsTradePeFinancial",
    "storageFactor",
    "subConvergenceRollback",
    "taxConvergence",
    "taxConvergenceRollback",
    "tradecost"
  )) {
    # source data for these types is in H12 regions
    # use the region data for each country that belongs to the region
    # no weighting for spatial aggregation

    # Replacing NA values with zero
    x[is.na(x)] <- 0

    if (subtype == "storageFactor") {
      mapping <- toolGetMapping(type = "regional", name = "regionmapping_21_EU11.csv", where = "mappingfolder")
    } else {
      mapping <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder")
    }

    out <- toolAggregate(x,
      rel = mapping, weight = NULL,
      from = "RegionCode", to = "CountryCode", partrel = TRUE
    ) %>%
      toolCountryFill(fill = 0, verbosity = 2)
  } else {
    out <- x
  }

  return(out)
}
