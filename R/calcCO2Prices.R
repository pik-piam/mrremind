calcCO2Prices <- function() {

  # read data
  x <- readSource("ExpertGuess", subtype = "co2prices")
  getNames(x) <- NULL

  # read data used for weight
  ceds <- calcOutput("Emissions", datasource = "CEDS2024", aggregate = FALSE)
  ceds <- ceds[, , "Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)"]

  # if most recent year is not in ceds, use latest ceds year that is available
  cedsyears <- getYears(ceds, as.integer = TRUE)
  xyears <- getYears(x, as.integer = TRUE)
  cedsmissing <- setdiff(xyears, cedsyears)
  if (length(cedsmissing) == 1 && cedsmissing > max(cedsyears)) {
    cedsyears <- c(intersect(cedsyears, xyears), max(cedsyears))
    ceds <- ceds[, cedsyears, ]
    getYears(ceds) <- xyears
  } else if (length(cedsmissing) > 1) {
    stop("CEDS data not available for ", paste(cedsmissing, collapse = ", "))
  }

  return(list(x           = x,
              weight      = ceds,
              unit        = "US$2005/t CO2",
              description = "CO2 prices in 2010, 2015 and 2020"))
}
