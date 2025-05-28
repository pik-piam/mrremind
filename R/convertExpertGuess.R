#' @title convertExpertGuess
#' @description Converts data from expert guess
#' @param x unconverted magpie object from read-script
#' @param subtype Type of data that are converted.
#'
#'
convertExpertGuess <- function(x, subtype) {
  if (subtype == "costsTradePeFinancial") {
    # use data for each country that belongs to a region
    # No weighting for spatial aggregation
    out <- toolAggregate(x, toolGetMapping(
      type = "regional", name = "regionmappingH12.csv",
      returnPathOnly = TRUE, where = "mappingfolder"
    ),
    weight = NULL
    )
  } else if (subtype %in% c("taxConvergenceRollback", "subConvergenceRollback")) {
    mapping <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder")
    out <- toolAggregate(x, mapping, weight = NULL, from = "RegionCode", to = "CountryCode", partrel = TRUE) %>%
      toolCountryFill(fill = 0)
  } else {
    out <- x
  }

  return(out)
}
