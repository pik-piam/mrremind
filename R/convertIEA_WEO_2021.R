#' Disaggregates IEA WEO 2021 Data
#' @param x MAgPIE object to be converted
#' @return A [`magpie`][magclass::magclass] object.
#' @param subtype Either "global" or "region". On global level, the source offers
#' more variables than on regional level, but the data should not be used on sub-
#' global level due to its coarse disaggregation.
#' @author Falk Benke
#' @importFrom madrat getISOlist
#'

convertIEA_WEO_2021 <- function(x, subtype = "global") { # nolint
  pe <- calcOutput("PE", aggregate = FALSE)
  if (subtype == "global") {
    # for now, we only have complete data on global level
    xWorld <- x["World", , ]

    # remove all-NA variables
    remove <- magpply(xWorld, function(y) all(is.na(y)), MARGIN = 3)
    xWorld <- xWorld[, , !remove]

    # to integrate the data in historical.mif, we need to disaggregate to country level
    # the disaggregation is very unprecise and therefore values below global granularity
    # are not reliable

    mappingWorld <- tibble(
      regions = "World",
      country = getISOlist()
    )

    weight <- pe[, 2014, "PE (EJ/yr)"]
    xWorld <- toolAggregate(xWorld, rel = mappingWorld, weight = weight)
    return(xWorld)
  } else if (subtype == "region") {
    .removeNaRegions <- function(x) {
      remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
      return(x[!remove, , ])
    }

    mappingFull <- toolGetMapping("regionmapping_IEA_WEO_2021.csv", type = "regional", where = "mappingfolder")

    .disaggregateRegions <- function(xIn, regionsIn) {
      x <- .removeNaRegions(xIn)

      regions <- intersect(regionsIn, getItems(x, dim = 1))

      if (length(regions) == 0) {
        return(toolCountryFill(x, fill = NA, verbosity = 2))
      }

      # ISO countries in x and the corresponding mapping
      ctry <- setdiff(getItems(x, dim = 1), regions)
      mappingCtry <- mappingFull[mappingFull$ISO3.code %in% ctry &
        mappingFull$Region_name %in% regions, ]

      # subtract country values in x from region values
      # e.g. USA from North America, if both are in data
      xSub <- x[mappingCtry$ISO3.code, , ]
      getItems(xSub, dim = 1) <- mappingCtry$Region_name
      x[unique(mappingCtry$Region_name), , ] <- x[unique(mappingCtry$Region_name), , ] - dimSums(xSub, dim = 3)

      # mapping of regions to ISO countries other than in ctry (i.e. other regions)
      mappingRegions <- mappingFull[mappingFull$Region_name %in% regions &
        !mappingFull$ISO3.code %in% ctry & mappingFull$ISO3.code != "SUN", ]

      # regions fully covered by country values can be removed
      coveredRegions <- setdiff(regions, unique(mappingRegions$Region_name))

      if (length(coveredRegions) > 0) {
        x <- x[coveredRegions, , invert = TRUE]
        regions <- setdiff(regions, coveredRegions)
      }

      weight <- pe[mappingRegions$ISO3.code, 2014, "PE (EJ/yr)"]

      # disaggregation of other regions to ISO countries
      x2 <- toolAggregate(x[regions, , ], rel = mappingRegions, weight = weight)

      # ISO countries in x that do not need to be disaggregated
      x1 <- x[regions, , invert = TRUE]

      if (length(getItems(x1, dim = 1)) == 0) {
        return(toolCountryFill(x2, fill = NA, verbosity = 2))
      }

      # combine the two objects
      x <- mbind(x1, x2)
      x <- toolCountryFill(x, fill = NA, verbosity = 2)

      return(x)
    }

    # exclude all regions we don't want to disaggregate due to redundancies,
    # low relevance, or lack of accuracy
    xReg <- x[c(
      "Atlantic Basin", "East of Suez", "NonOPEC", "OPEC", "Japan and Korea",
      "Southeast Asia", "Other", "European Union", "World",
      "Advanced economies", "Emerging market and developing economies",
      "International Energy Agency", "OECD", "Non-OECD",
      "North Africa", "Sub-Saharan Africa", "Rest of world",
      "Other Asia Pacific", "Other Europe"
    ), , , invert = TRUE]

    # remove all-NA variables
    remove <- magpply(xReg, function(y) all(is.na(y)), MARGIN = 3)
    xReg <- xReg[, , !remove]

    regions <- c(
      "Africa", "Asia Pacific", "Central and South America", "Europe",
      "Eurasia", "Middle East", "North America"
    )
    x1 <- xReg[regions, , ]

    # convert country names to ISO
    ctry <- toolCountry2isocode(getItems(xReg, dim = 1), warn = FALSE)
    x2 <- xReg[!is.na(ctry), , ]
    getItems(x2, dim = 1) <- ctry[!is.na(ctry)]
    xReg <- mbind(x1, x2)

    xRegional <- NULL
    for (i in getItems(xReg, dim = 3)) {
      j <- xReg[, , i]
      j <- .removeNaRegions(j)
      xRegional <- mbind(xRegional, .disaggregateRegions(xIn = j, regionsIn = regions))
    }

    non28EUcountries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")
    xRegional[non28EUcountries, , ] <- 0

    return(xRegional)
  } else {
    stop("Not a valid subtype! Must be either \"region\" or \"global\"")
  }
}
