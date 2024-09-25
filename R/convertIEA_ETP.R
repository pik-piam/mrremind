#' Convert IEA ETP projections
#'
#' @author Falk Benke
#' @param x IEA ETP projection magpie object derived from readIEA_ETP function
#' @param subtype data subtype. Either "industry", "buildings", "summary", or "transport"
#'
convertIEA_ETP <- function(x, subtype) {

  map <- toolGetMapping("regionmappingIEA_ETP.csv", where = "mrremind", type = "regional")
  fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)

  # disaggregate ASEAN
  x.asean <- x["ASEAN", , ]
  m <- select(map, c("EEAReg", "CountryCode")) %>% filter(.data$EEAReg == "ASEAN")
  w <- fe[m$CountryCode, 2005, "FE|Transport (EJ/yr)"]
  x.asean <- toolAggregate(x.asean, m, from = "EEAReg", to = "CountryCode", weight = w)

  # disaggregate European Union
  x.eu <- x["European Union", , ]
  m <- select(map, c("EEAReg", "CountryCode")) %>% filter(.data$EEAReg == "EUR")
  w <- fe[m$CountryCode, 2005, "FE|Transport (EJ/yr)"]
  x.eu <- toolAggregate(x.eu, m, from = "EEAReg", to = "CountryCode", weight = w)

  # transform entries that don't require disaggregation
  x.ctry <- x[c("Brazil", "China", "India", "Mexico", "Russia", "South Africa", "United States"), , ]
  getItems(x.ctry, dim = 1) <- toolCountry2isocode(getItems(x.ctry, dim = 1), warn = FALSE)


  x <- mbind(x.asean, x.eu, x.ctry)
  x <- toolCountryFill(x, fill = NA, verbosity = 2)

  return(x)
}
