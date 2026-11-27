#' Convert EDGEtransport
#'
#' @param x MAgPIE object containing EDGE-T values in 21 region resolution
#' @inheritParams readEDGETransport
#' @return REMIND/iterative EDGE-T input data as MAgPIE object disaggregated to ISO level
#' @author Johanna Hoppe
#'
convertEDGETransport <- function(x, subtype) {

  mappingfile <- toolGetMapping("regionmapping_21_EU11.csv", type = "regional", where = "mappingfolder")

  if (subtype == "shares_LDV_transport") {
    return(x)
  } else if (subtype %in% c("f35_demByTech", "f29_trpdemand", "weightESdemand")) {
    weight <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE) %>%
      time_interpolate(getYears(x), extrapolation_type = "constant")

  } else {
    weight <- NULL
  }

  result <- toolAggregate(x = x, weight = weight, rel = mappingfile, from = "RegionCode", to = "CountryCode")

  return(result)
}
