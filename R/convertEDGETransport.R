#' Convert EDGEtransport
#'
#' Ship EDGETransport data through, as already on ISO level
#'
#' @param subtype EDGE entries
#' @param x MAgPIE object containing EDGE values at ISO country resolution
#' @return EDGETransport data as MAgPIE object disaggregated to ISO level
#' @author Marianna Rottoli
#'
#' @importFrom data.table setDT
#'
convertEDGETransport = function(x, subtype) {

  mappingfile <- setDT(toolGetMapping("regionmapping_21_EU11.csv", type = "regional",
                                      where = "mappingfolder"))[, .(iso = CountryCode, region = RegionCode)]
  if subtype %in% c() {
    gdp <- calcOutput("GDP", aggregate = F)
    x = toolAggregate(x = x, weight = gdp, rel = mappingfile, from = "region", to = "iso")
  } else {
    result <- toolAggregate(x = x, rel = mappingfile, weight = NULL, from = "region", to = "iso")
  }

  return(result)
}
