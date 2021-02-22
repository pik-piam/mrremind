#' @importFrom madrat toolGetMapping

convertINNOPATHS <- function(x) {

  regmapping <- toolGetMapping("regionmapping_21_EU11.csv", type = "regional")
  
  # calculate GDP per Capita to be used as weight for disaggregation
  # TODO: revise this weight in case a bigger disaggregation than the 21 regions 
  # remind-eu disaggregation should be introduced
  GDPppp <- calcOutput("GDPppp", GDPpppFuture = "SSP", aggregate = FALSE)
  population <- calcOutput("Population", aggregate = FALSE)
  gdpPerCapita <- GDPppp[,,"gdp_SSP2"] / population[,,"pop_SSP2"]
  
  # ensure that regions match for disaggregation
  commonRegions <- intersect(getRegions(x), regmapping$RegionCode)
  regmapping <- regmapping %>% filter(regmapping$RegionCode %in% commonRegions)
  x <- x[commonRegions, , ]
  w <- gdpPerCapita[regmapping$CountryCode, 2005, ]

  x <- toolAggregate(x, regmapping, from = "RegionCode", to = "CountryCode", weight = w)
  x <- toolCountryFill(x)

  return(x)
}
