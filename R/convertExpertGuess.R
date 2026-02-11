#' @title convertExpertGuess
#' @description Converts data from expert guess
#' @param x unconverted magpie object from read-script
#' @inheritParams readExpertGuess
#'
convertExpertGuess <- function(x, subtype) {


  if (subtype %in% c(
    "capacityFactorRules",
    "costsTradePeFinancial",
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

    mapping <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder")

    out <- toolAggregate(x, rel = mapping, weight = NULL,
                         from = "RegionCode", to = "CountryCode", partrel = TRUE) %>%
      toolCountryFill(fill = 0, verbosity = 2)

  } else if (subtype == "deltacapoffset") {

    weight <- dimSums(calcOutput("IO", subtype = "output", aggregate = FALSE)[, 2010, c("feelb", "feeli")], dim = 3)
    mapping <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder")
    out <- toolAggregate(x, rel = mapping, weight = weight, from = "RegionCode", to = "CountryCode")

  } else {
    out <- x
  }

  return(out)
}
