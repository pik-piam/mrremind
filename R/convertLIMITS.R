convertLIMITS <- function(x, subtype) {

  # Parameter definitions
  # TODO: Is it consistent to disaggregate activity and emission data using 2005 SSP2 pop data?
  p_dagg_year <- 2005
  p_dagg_pop  <- "pop_SSP2"

  # Check errors
  if (is.null(subtype)) stop("Please provide one of the following subtypes: 'activities' or 'emissions'")

  # For now, this is useless as the processing for activities and emissions is the same
  if (subtype == "activities") {

    m <- toolGetMapping(type = "regional", name = "regionmappingTIMER.csv", returnPathOnly = TRUE, where = "mappingfolder")

    # Get TIMER regional mapping
    map <- read.csv2(m)
    map <- map[!(map$RegionCode == "" | map$CountryCode == "ANT"), c(2, 3)]
    map$CountryCode <- factor(map$CountryCode)
    map$RegionCode  <- factor(map$RegionCode)

    w <- calcOutput("Population", aggregate = FALSE)[levels(map$CountryCode), p_dagg_year, p_dagg_pop]
    x <- toolAggregate(x[, , ], map, weight = w)
  }

  if (subtype == "emissions") {

    m <- toolGetMapping(type = "regional", name = "regionmappingTIMER.csv", returnPathOnly = TRUE, where = "mappingfolder")

    map <- read.csv2(m)
    map <- map[!(map$RegionCode == "" | map$CountryCode == "ANT"), c(2, 3)]
    map$CountryCode <- factor(map$CountryCode)
    map$RegionCode  <- factor(map$RegionCode)

    w <- calcOutput("Population", aggregate = FALSE)[levels(map$CountryCode), p_dagg_year, p_dagg_pop]
    x <- toolAggregate(x[, , ], map, weight = w)
  }

  # fill all missing countries with 0
  x <- toolCountryFill(x, fill = 0)

  return(x)
}
