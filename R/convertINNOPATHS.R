#' @importFrom madrat toolGetMapping
#' @importFrom dplyr %>%
convertINNOPATHS <- function(x) {

  regmapping <- toolGetMapping("regionmapping_21_EU11.csv", type = "regional", where = "mappingfolder")

  gdpPerCapita <- calcOutput("GDPpc", aggregate = F)
  
  # ensure that regions match for disaggregation
  commonRegions <- intersect(getItems(x, dim = 1), regmapping$RegionCode)
  
  # restrict disaggregation to EU28 countries
  Non28EUcountries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")

  regmapping <- regmapping %>% filter(
    !!sym("RegionCode") %in% commonRegions,
    !(!!sym("CountryCode") %in% Non28EUcountries)
  )
  x <- x[commonRegions, , ]
  w <- gdpPerCapita[regmapping$CountryCode, 2005, "gdppc_SSP2EU"]

  x <- toolAggregate(x, regmapping, from = "RegionCode", to = "CountryCode", weight = w)
  
  x <- toolCountryFill(x, fill = NA, verbosity = 2)
  x[Non28EUcountries,,] <- 0
  return(x)
}
