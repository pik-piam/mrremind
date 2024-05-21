#' Convert IEA WEO 2021 Data
#'
#' @param x magclass object to be converted
#' @param subtype "2021-global", "2021-region", "2023-global", or "2023-region".
#' - For 2021 we have complete paid data. For 2023 we have only the free dataset.
#' - On global level, the source offers more variables than on regional level,
#' but the data should not be used on sub-global level due to its coarse disaggregation.
#' @author Falk Benke
#'

convertIEA_WorldEnergyOutlook <- function(x, subtype = "2021-global") { # nolint

  pe <- calcOutput("PE", aggregate = FALSE)

  if (grepl("-global$", subtype)) {

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

  } else if (grepl("-region$", subtype)) {

    .removeNaRegions <- function(x) {
      remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
      return(x[!remove, , ])
    }

    mappingFull <- toolGetMapping("regionmapping_IEA_WEO_2021.csv",
                                  type = "regional", where = "mappingfolder")

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
      "Other Asia Pacific", "Other Europe", "Non-OPEC"
    ), , , invert = TRUE]

    # remove all-NA variables
    remove <- magpply(xReg, function(y) all(is.na(y)), MARGIN = 3)
    xReg <- xReg[, , !remove]

    # regions we disaggregate
    regions <- c(
      "Africa", "Asia Pacific", "Central and South America", "Europe",
      "Eurasia", "Middle East", "North America"
    )
    x1 <- xReg[regions, , ]

    # convert country names to ISO
    x2 <- xReg[regions, , , invert = TRUE]
    getItems(x2, dim = 1) <- toolCountry2isocode(getItems(x2, dim = 1), warn = TRUE)

    xReg <- mbind(x1, x2)

    xRegional <- NULL

    # disaggregate regions per variable
    for (v in getItems(xReg, dim = 3)) {
      xRegional <- mbind(
        xRegional,
        .disaggregateRegions(xIn = xReg[, , v], regionsIn = regions)
      )
    }

    xRegional <- toolFillEU34Countries(xRegional)

    return(xRegional)

  } else {
    stop("Not a valid subtype! Must be one of: '2021-region', '2021-global', '2023-region', '2023-global'")
  }
}
