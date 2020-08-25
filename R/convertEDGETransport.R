#' Convert EDGEtransport
#'
#' Ship EDGETransport data through, as already on ISO level
#'
#' @param subtype EDGE entries
#' @param x MAgPIE object containing EDGE values at ISO country resolution
#' @return EDGETransport data as MAgPIE object aggregated to ISO level
#' @author Marianna Rottoli
#'

convertEDGETransport = function(x, subtype) {

  if (subtype %in% c("esCapCost", "fe_demand_tech", "fe2es", "UCD_NEC_iso", "harmonized_intensities", "value_time", "SW", "pref", "loadFactor")) {
    ## magpie object creates NA whenever the initial dt is not symmetric (entry absent in ISO1 but exists in ISO2)
    ## the NAs are therefore converted to 0
    x[is.na(x)] <- 0
  }

  if (subtype %in% c("shares_LDV_transport")) {
    ## only ConvCase (ICE predominant LDV market and road market) is used as input data
    x <- x[,,"ConvCase.share_LDV_totliq", pmatch = TRUE]

    for (year in getYears(x, as.integer = T)){
      x[,year,] <- as.vector(x[,c(2010),]) + ((0.55 - as.vector(x[,c(2010),]))/(2100-2010))*(year-2010)
    }

    #extending values
    x <- time_interpolate(x, integrate_interpolated_years=T, interpolated_year = seq(from = 1990, to = 2100), extrapolation_type = "linear")
    x <- time_interpolate(x, integrate_interpolated_years=T, interpolated_year = c(seq(from = 1970, to = 1989),seq(from = 2101, to = 2150)), extrapolation_type = "constant")
  }

  result = x
  return(result)
}
