#' Convert IEA EV Outlook
#'
#' @param x a magclass object returned from `readIEA_EVOutlook()`
#' @author Falk Benke

convertIEA_EVOutlook <- function(x) {

  # remove regions that require disaggregation
  data <- x[c("World", "Rest of the world", "Europe", "EU27"), , invert = TRUE]
  getItems(data, dim = 1) <- toolCountry2isocode(getItems(data, dim = 1))

  # disaggregate "Europe"
  europe <- x["Europe", , ]

  nonEU28Countries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")

  mapping <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", where = "mappingfolder") %>%
    filter(.data$RegionCode == "EUR", !(.data$CountryCode %in% nonEU28Countries))

  w <- calcOutput("GDPPast", aggregate = FALSE)[, "y2015", ]

  europe <- toolAggregate(
    europe, mapping,
    from = "RegionCode", to = "CountryCode",
    dim = 1, weight = w[mapping$CountryCode, , ]
  )

  # for each variable with no finer granularity than 'Europe'
  # use disaggregation of Europe to 28 countries
  data <- magclass::add_columns(data, addnm = "MLT", dim = 1, fill = NA)

  for (v in getNames(europe)) {
    if (all(is.na(data[getItems(europe, dim = 1), , v]))) {
      data[getItems(europe, dim = 1), , v] <- europe[, , v]
    }
  }

  data <- toolCountryFill(data, fill = NA, verbosity = 2) %>%
    toolFillEU34Countries()

  return(data)

}
