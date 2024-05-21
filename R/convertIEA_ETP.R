#' Convert IEA ETP projections
#'
#' @author Falk Benke
#' @param x IEA ETP projection magpie object derived from readIEA_ETP function
#' @param subtype data subtype. Either "industry", "buildings", "summary", or "transport"
#' @importFrom dplyr %>%
#'
convertIEA_ETP <- function(x, subtype) {

  getItems(x, dim = 1) <- lapply(getItems(x, dim = 1), function(y) ifelse(y == "NonOECD", "Non-OECD", y))

  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv", where = "mappingfolder", type = "regional")

  fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)

  v <- magpply(x[c("OECD", "Non-OECD"), , , invert = T], function(y) all(is.na(y)), MARGIN = 3)
  v.oecd.only <- getNames(x[, , v])

  if (is.null(v.oecd.only)) {
    v.full <- getNames(x)
  } else {
    v.full <- getNames(x[, , v.oecd.only, invert = T])
  }

  # disaggregate ASEAN
  x.asean <- x["ASEAN", , v.full]
  m <- select(regmapping, c("EEAReg", "CountryCode")) %>% filter(!!sym("EEAReg") == "ASEAN")
  w <- fe[m$CountryCode, 2005, "FE|Transport (EJ/yr)"]
  x.asean <- toolAggregate(x.asean, m, from = "EEAReg", to = "CountryCode", weight = w)

  # disaggregate European Union
  x.eu <- x["European Union", , v.full]
  m <- select(regmapping, c("EEAReg", "CountryCode")) %>% filter(!!sym("EEAReg") == "EUR")
  w <- fe[m$CountryCode, 2005, "FE|Transport (EJ/yr)"]
  x.eu <- toolAggregate(x.eu, m, from = "EEAReg", to = "CountryCode", weight = w)

  # entries that don't require disaggregation
  x.ctry <- x[c("Brazil", "China", "India", "Mexico", "Russia", "South Africa", "United States"), , v.full]
  getItems(x.ctry, dim = 1) <- toolCountry2isocode(getItems(x.ctry, dim = 1), warn = F)

  # disaggregate OECD data for variables with both OECD and finer regional granularity
  x.oecd.other <- x[c("OECD", "Non-OECD"), , v.full]

  # get OECD/Non-OECD values not accounted for in other regions
  x.oecd.other["OECD", , ] <- x.oecd.other["OECD", , ] -
    dimSums(x[c("European Union", "Mexico", "United States"), , v.full], dim = 1)
  x.oecd.other["Non-OECD", , ] <- x.oecd.other["Non-OECD", , ] -
    dimSums(x[c("ASEAN", "Brazil", "China", "India", "Russia", "South Africa"), , v.full], dim = 1)
  m <- select(regmapping, c("OECD", "CountryCode")) %>%
    filter(!(!!sym("CountryCode") %in% c(getItems(x.asean, dim = 1), getItems(x.eu, dim = 1), getItems(x.ctry, dim = 1))))
  w <- fe[m$CountryCode, 2005, "FE|Transport (EJ/yr)"]
  x.oecd.other <- toolAggregate(x.oecd.other, m, from = "OECD", to = "CountryCode", weight = w)

  x.full <- new.magpie(getISOlist(), getYears(x), names = getNames(x), fill = 0)

  # for variables with only OECD/nonOECD data we disaggregate this to country-level
  if (!is.null(v.oecd.only)) {
    w <- fe[regmapping$CountryCode, 2005, "FE|Transport (EJ/yr)"]
    x.oecd <- x[c("OECD", "Non-OECD"), , v.oecd.only]
    x.oecd <- toolAggregate(x.oecd, regmapping, from = "OECD", to = "CountryCode", weight = w)
    x.full[getItems(x.oecd, dim = 1), , v.oecd.only] <- x.oecd
  }

  # for variables with OECD/nonOECD data and addtl. region and country data
  # 1) we disaggregate the more fine-granular regions ASEAN and European Union first
  # 2) then calculate the values for the OECD/nonOECD regions minus the values for explicitly listed countries/regions
  # and disaggregate them to the countries not listed explicitly
  x.full[getItems(x.asean, dim = 1), , v.full] <- x.asean
  x.full[getItems(x.eu, dim = 1), , v.full] <- x.eu
  x.full[getItems(x.ctry, dim = 1), , v.full] <- x.ctry
  x.full[getItems(x.oecd.other, dim = 1), , v.full] <- x.oecd.other

  return(x.full)
}
