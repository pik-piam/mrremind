#' Convert EDGEtransport
#'
#' @param subtype REMIND/iterative EDGE-T input data subtypes
#' @param x MAgPIE object containing EDGE-T values in 21 region resolution
#' @return REMIND/iterative EDGE-T input data as MAgPIE object disaggregated to ISO level
#' @importFrom data.table setDT
#' @importFrom madrat toolAggregate
#' @author Johanna Hoppe

convertEDGETransport <- function(x, subtype) {

  RegionCode <- CountryCode <- . <- NULL

  mappingfile <- setDT(toolGetMapping("regionmapping_21_EU11.csv", type = "regional",
                                      where = "mappingfolder"))[, .(iso = CountryCode, region = RegionCode)]
  if (subtype %in% c("f35_demByTech", "f29_trpdemand", "weightESdemand")) {
    gdp <- calcOutput("GDP", aggregate = FALSE) |> time_interpolate(getYears(x), extrapolation_type = "constant")
    gdp <- gdp[, , "gdp_SSP2"]
    result <- toolAggregate(x = x, weight = gdp, rel = mappingfile, from = "region", to = "iso")
  } else if (!subtype == "shares_LDV_transport") {
    result <- toolAggregate(x = x, rel = mappingfile, weight = NULL, from = "region", to = "iso")
  } else if (subtype %in% c("shares_LDV_transport")) {
    ## only the first EDGE-T scenario for SSP2 is used as a proxy for the LDV shares
    x <- x[, , "gdp_SSP2.Mix1.gdp_SSP2.share_LDV_totliq.shares_LDV_transport"]

    for (year in getYears(x, as.integer = TRUE)) {
      x[, year, ] <- as.vector(x[, c(2010), ]) + ((0.55 - as.vector(x[, c(2010), ])) / (2100 - 2010)) * (year - 2010)
    }
    # extending values
    x <- time_interpolate(x, integrate_interpolated_years = TRUE, interpolated_year = seq(from = 1990, to = 2100), extrapolation_type = "linear")
    x <- time_interpolate(x, integrate_interpolated_years = TRUE, interpolated_year = c(seq(from = 1970, to = 1989), seq(from = 2101, to = 2150)), extrapolation_type = "constant")
    result <- x
  }
  return(result)
}
