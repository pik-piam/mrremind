convertIEA_EVOutlook <- function(x) {
  
  Non28EUcountries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")
  w <- calcOutput("GDPPast", aggregate = F)[, "y2015", ]
  mapping <- toolGetMapping(type = "regional", name = "regionmappingH12.csv")

  .removeNaRegions <- function(x) {
    remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
    return(x[!remove, , ])
  }

  .disaggregate_other_europe <- function(x1, var) {
    
    # get eu countries that do not belong to "Other Europe" for this variable
    eu.countries <- getItems(.removeNaRegions(x[, , var]), dim = 1)
    
    # get mapping to "Other Europe"
    m <- mapping %>%
      filter(
        !!sym("RegionCode") == "EUR",
        !(!!sym("CountryCode") %in% c(eu.countries, Non28EUcountries))
      ) %>%
      mutate(!!sym("Reg") := "Other Europe")

    y <- toolAggregate(x1, m,
      from = "Reg", to = "CountryCode",
      dim = 1, weight = w[unique(m$CountryCode), , ]
    )
    return(y)
  }

  # we attempt to disaggregate "Other Europe" per variable to the EU28 countries
  other.europe <- x["Other Europe", , ]
  na.vars <- magpply(other.europe, function(y) all(is.na(y)), MARGIN = 3)
  other.europe <- other.europe[, , !na.vars]

  # remove regions that require disaggregation
  x <- x[c("World", "Rest of the world", "Europe", "Other Europe"), , invert = TRUE]
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))

  varlist <- list(x)

  # disaggregate "Other Europe" where applicable
  for (var in getNames(other.europe)) {
    varlist <- append(varlist, list(.disaggregate_other_europe(other.europe[, , var], var)))
  }

  # merge magclass objects into one
  years <- sort(Reduce(union, lapply(varlist, getYears)))
  regions <- Reduce(union, lapply(varlist, getRegions))
  data <- new.magpie(regions, years, getNames(x), fill = NA)
  for (i in varlist) {
    data[getRegions(i), getYears(i), getNames(i)] <- i
  }

  data <- toolCountryFill(data, fill = NA, verbosity = 2)
  
  # set small EU countries to 0 to allow for aggregation of EU region
  data[Non28EUcountries, , ] <- 0
  
  return(data)
}
