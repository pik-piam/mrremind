calcCO2Prices <- function() {

  # read data
  x <- readSource("ExpertGuess", subtype = "co2prices")
  getNames(x) <- NULL


  # convert from $2005 to $2017
  map <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder") %>%
    select("iso3c" = "CountryCode", "region" = "RegionCode")

  x <- GDPuc::convertGDP(
    gdp = x,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2017 Int$PPP",
    with_regions = map,
    replace_NAs = "regional_average"
  )

  # read data used for weight
  ceds <- calcOutput("Emissions", datasource = "CEDS2024", aggregate = FALSE)
  ceds <- ceds[, , "Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"]

  ceds <- ceds[, getYears(x), ]

  return(list(
    x = x,
    weight = ceds,
    unit = "US$2017/t CO2",
    description = "CO2 prices in 2010, 2015 and 2020"
  ))
}
