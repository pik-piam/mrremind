convertLIMITS <- function(x, subtype) {
  # Check errors
  if (is.null(subtype)) {
    stop("Please provide one of the following subtypes: 'activities' or 'emissions'")
  }

  m <- toolGetMapping(type = "regional", name = "regionmappingTIMER.csv", returnPathOnly = TRUE, where = "mappingfolder")

  # Get TIMER regional mapping
  map <- utils::read.csv2(m)
  map <- map[!(map$RegionCode == "" | map$CountryCode == "ANT"), c(2, 3)]
  map$CountryCode <- factor(map$CountryCode)
  map$RegionCode  <- factor(map$RegionCode)

  # TODO: Is it consistent to disaggregate activity and emission data using 2005 SSP2 pop data?
  w <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[levels(map$CountryCode), 2005, ]
  x <- toolAggregate(x[, , ], map, weight = w)

  # fill all missing countries with 0
  toolCountryFill(x, fill = 0, verbosity = 2)
}
