#' Convert EDGEtransport
#'
#' @param subtype REMIND/iterative EDGE-T input data subtypes
#' @param x MAgPIE object containing EDGE-T values in 21 region resolution
#' @return REMIND/iterative EDGE-T input data as MAgPIE object disaggregated to ISO level
#' @author Johanna Hoppe
#'
convertEDGETransport <- function(x, subtype) {

  RegionCode <- CountryCode <- . <- NULL

  mappingfile <- data.table::setDT(toolGetMapping("regionmapping_21_EU11.csv", type = "regional",
                                                  where = "mappingfolder"))[, .(iso = CountryCode, region = RegionCode)]
  if (subtype %in% c("f35_demByTech", "f29_trpdemand", "weightESdemand")) {

    gdp <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE) |>
      time_interpolate(getYears(x), extrapolation_type = "constant")

    result <- toolAggregate(x = x, weight = gdp, rel = mappingfile, from = "region", to = "iso")

  } else {

    result <- toolAggregate(x = x, rel = mappingfile, weight = NULL, from = "region", to = "iso")

  }

  return(result)
}
