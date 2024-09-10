calcCO2Prices <- function() {

  # read data
  x <- readSource("ExpertGuess", subtype = "co2prices")

  # convert from $2005 to $2017

  x <- GDPuc::convertGDP(
    gdp = x,
    unit_in = "constant 2005 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )

  getNames(x) <- NULL

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
