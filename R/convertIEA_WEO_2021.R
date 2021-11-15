convertIEA_WEO_2021 <- function(x) {
  .removeNaRegions <- function(x) {
    remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
    return(x[!remove, , ])
  }

  PE <- calcOutput("PE", aggregate = FALSE)
  mapping_full <- toolGetMapping("regionmapping_IEA_WEO_2021.csv", type = "regional")

  .disaggregate_regions <- function(x_in, regions_in) {
    x <- .removeNaRegions(x_in)

    regions <- intersect(regions_in, getItems(x, dim = 1))

    # iso countries in x
    ctry <- toolCountry2isocode(getItems(x, dim = 1), warn = F)
    ctry <- ctry[!is.na(ctry)]

    # mapping of regions to iso countries other than in ctry (i.e. other regions)
    mapping_regions <- mapping_full[mapping_full$Region_name %in% regions &
      !mapping_full$ISO3.code %in% ctry & mapping_full$ISO3.code != "SUN", ]

    weight <- PE[mapping_regions$ISO3.code, 2016, "PE (EJ/yr)"]

    # disaggregation of other regions to iso countries
    x2 <- toolAggregate(x[regions, , ], rel = mapping_regions, weight = weight)
    x2 <- toolCountryFill(x2, fill = 0, verbosity = 2)
    x2[is.na(x2)] <- 0

    # iso countries in x that do not need to be disaggregated
    x1 <- x[regions, , invert = TRUE]

    if (length(getRegions(x1)) == 0) {
      return(x2)
    }

    getItems(x1, dim = 1) <- toolCountry2isocode(getItems(x1, dim = 1), warn = F)
    x1 <- toolCountryFill(x1, fill = 0, verbosity = 2)
    x1[is.na(x1)] <- 0
    # combine the two objects
    x <- x1 + x2

    return(x)
  }

  # exclude all regions we don't want to disaggregate due to reduncancies or lack of accuracy
  x <- x[c(
    "Atlantic Basin", "East of Suez", "NonOPEC", "OPEC", "Japan and Korea",
    "Eurasia", "Southeast Asia", "Other", "European Union", "World"
  ), , , invert = T]

  # remove all-na variables
  remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 3)
  x <- x[, , !remove]

  regions <- c("Africa", "Asia Pacific", "Central and South America", "Europe", "Middle East", "North America")

  x_disaggregated <- new.magpie(getISOlist(), getYears(x), names = NULL)

  for (i in getNames(x)) {
    j <- x[, , i]
    
    if ("United States" %in% getRegions(j) & "North America" %in% getRegions(j)) {
      j["North America", , ] <- j["North America", , ] - j["United States", , ]
    }

    if ("Brazil" %in% getRegions(j) & "Central and South America" %in% getRegions(j)) {
      j["Central and South America", , ] <- j["Central and South America", , ] - j["Brazil", , ]
    }

    if ("Japan" %in% getRegions(j) & "Asia Pacific" %in% getRegions(j)) {
      j["Asia Pacific", , ] <- j["Asia Pacific", , ] - j["Japan", , ]
    }
    
    if ("India" %in% getRegions(j) & "Asia Pacific" %in% getRegions(j)) {
      j["Asia Pacific", , ] <- j["Asia Pacific", , ] - j["India", , ]
    }
    
    if ("China" %in% getRegions(j) & "Asia Pacific" %in% getRegions(j)) {
      j["Asia Pacific", , ] <- j["Asia Pacific", , ] - j["China", , ]
    }
    
    x_disaggregated <- mbind(x_disaggregated, .disaggregate_regions(x_in = j, regions_in = regions))
  }

  return(x_disaggregated[, , "dummy", invert = T])
}
