#' @title convertExpertGuess
#' @description Converts data from expert guess
#' @param x unconverted magpie object from read-script
#' @inheritParams readExpertGuess
#' @author Falk Benke
#'
convertExpertGuess <- function(x, subtype) {

  # subtypes that require disaggregation

  mapping <- c(
    "capacityFactorRules" = "regionmappingH12.csv",
    "costsTradePeFinancial" = "regionmappingH12.csv",
    "storageFactor" = "regionmapping_21_EU11.csv",
    "subConvergenceRollback" = "regionmappingH12.csv",
    "taxConvergence" = "regionmappingH12.csv",
    "taxConvergenceRollback" = "regionmappingH12.csv"
  )

  if (subtype %in% names(mapping)) {

    # source data for these types is in H12 regions
    # use the region data for each country that belongs to the region
    # no weighting for spatial aggregation

    # Replacing NA values with zero
    x[is.na(x)] <- 0

    mapping <- toolGetMapping(type = "regional", name = mapping[[subtype]], where = "mappingfolder")

    out <- toolAggregate(x, rel = mapping, weight = NULL,
                         from = "RegionCode", to = "CountryCode", partrel = TRUE) %>%
      toolCountryFill(fill = 0, verbosity = 2)

  } else {
    out <- x
  }

  return(out)
}
