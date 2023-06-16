#' Convert EDGEtransport
#'
#' Ship EDGETransport data through, as already on ISO level
#'
#' @param subtype EDGE entries
#' @param x MAgPIE object containing EDGE values at ISO country resolution
#' @return EDGETransport data as MAgPIE object aggregated to ISO level
#' @author Marianna Rottoli
#'
#' @importFrom data.table setDT
#'
convertEDGETransport = function(x, subtype) {
  `.` <- CountryCode <- RegionCode <- NULL
  if (subtype %in% c("esCapCost", "fe_demand_tech", "fe2es", "UCD_NEC_iso", "harmonized_intensities", "value_time", "pref", "loadFactor", "annual_mileage", "f35_bunkers_fe")) {
    ## magpie object creates NA whenever the initial dt is not symmetric (entry absent in ISO1 but exists in ISO2)
    ## the NAs are therefore converted to 0
    x[is.na(x)] <- 0
  }
  ## load mapping
  mappingfile <- setDT(toolGetMapping("regionmapping_21_EU11.csv",type="regional", where = "mappingfolder"))[, .(iso = CountryCode, region = RegionCode)]
  ## for intensive values, the weight is NULL
  if (subtype %in% c("fe2es", "UCD_NEC_iso", "harmonized_intensities", "value_time", "pref", "loadFactor", "annual_mileage", "shares_LDV_transport", "price_nonmot", "esCapCost")) {
    x = toolAggregate(x = x, rel = mappingfile, weight = NULL, from = "region", to = "iso")
  }

  ## for extensive values and the edge_esm module, the weight is ES
  if (subtype == "fe_demand_tech") {
    wgt = readSource("EDGETransport", subtype ="demISO")
    ## rename the columns of the weight
    x = toolAggregate(x = x, weight = wgt, rel = mappingfile, from = "region", to = "iso")
  }

  ## for extensive values and the complex module, the weight is GDP
  if (subtype == "pm_fe_demand_EDGETbased") {
    gdp <- calcOutput("GDP", aggregate = F)[,,"gdp_SSP2"]
    ## interpolate missing time steps
    gdp <- time_interpolate(gdp, getYears(x))

    ## rename the columns of the weight
    getSets(gdp) <- c("iso", "year", "variable")
    x = toolAggregate(x = x, weight = gdp, rel = mappingfile, from = "region", to = "iso")
  }


  if (subtype %in% c("shares_LDV_transport")) {
    ## only the first EDGE-T scenario for SSP2 is used as a proxy for the LDV shares
    x <- x[,, "gdp_SSP2.Mix1.share_LDV_totliq.shares_LDV_transport"]

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
