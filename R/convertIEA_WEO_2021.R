#' Disaggregates IEA WEO 2021 Data
#' @param x MAgPIE object to be converted
#' @return A [`magpie`][magclass::magclass] object.
#' @param subtype Either "global" or "region"
#' @author Falk Benke
#' @importFrom madrat getISOlist
#'

convertIEA_WEO_2021 <- function(x, subtype = "global") {
  PE <- calcOutput("PE", aggregate = FALSE)
  if (subtype == "global") {

    # for now, we only have complete data on global level
    x.world <- x["World", , ]

    # to integrate the data in historical.mif, we need to disaggregate to country level
    # the disaggregation is very unprecise and therefore values below global granularity
    # are not reliable

    mapping_world <- tibble(
      regions = "World",
      country = getISOlist()
    )

    weight <- PE[, 2014, "PE (EJ/yr)"]
    x.world <- toolAggregate(x.world, rel = mapping_world, weight = weight)
    return(x.world)
  } else if (subtype == "region") {
    .removeNaRegions <- function(x) {
      remove <- magpply(x, function(y) all(is.na(y)), MARGIN = 1)
      return(x[!remove, , ])
    }

    mapping_full <- toolGetMapping("regionmapping_IEA_WEO_2021.csv", type = "regional")

    .disaggregate_regions <- function(x_in, regions_in) {
      x <- .removeNaRegions(x_in)

      regions <- intersect(regions_in, getItems(x, dim = 1))

      # iso countries in x
      ctry <- toolCountry2isocode(getItems(x, dim = 1), warn = FALSE)
      ctry <- ctry[!is.na(ctry)]

      # mapping of regions to iso countries other than in ctry (i.e. other regions)
      mapping_regions <- mapping_full[mapping_full$Region_name %in% regions &
        !mapping_full$ISO3.code %in% ctry & mapping_full$ISO3.code != "SUN", ]

      weight <- PE[mapping_regions$ISO3.code, 2014, "PE (EJ/yr)"]

      # disaggregation of other regions to iso countries
      x2 <- toolAggregate(x[regions, , ], rel = mapping_regions, weight = weight)

      # iso countries in x that do not need to be disaggregated
      x1 <- x[regions, , invert = TRUE]

      if (length(getRegions(x1)) == 0) {
        return(toolCountryFill(x2, fill = NA, verbosity = 2))
      }

      getItems(x1, dim = 1) <- toolCountry2isocode(getItems(x1, dim = 1), warn = FALSE)

      # combine the two objects
      x <- mbind(x1, x2)
      x <- toolCountryFill(x, fill = NA, verbosity = 2)

      return(x)
    }

    # exclude all regions we don't want to disaggregate due to redundancies or lack of accuracy
    x.reg <- x[c(
      "Atlantic Basin", "East of Suez", "NonOPEC", "OPEC", "Japan and Korea",
      "Southeast Asia", "Other", "European Union", "World"
    ), , , invert = TRUE]

    # remove all-na variables
    remove <- magpply(x.reg, function(y) all(is.na(y)), MARGIN = 3)
    x.reg <- x.reg[, , !remove]

    # remove 2040 as year, as source has no regional data for this year
    x.reg <- x.reg[, 2040, , invert = TRUE]

    regions <- c("Africa", "Asia Pacific", "Central and South America", "Europe",
                 "Eurasia", "Middle East", "North America")

    x.regional <- new.magpie(getISOlist(), getYears(x.reg), names = NULL)

    for (i in getNames(x.reg)) {
      j <- x.reg[, , i]

      j <- .removeNaRegions(j)

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

      if ("Russia" %in% getRegions(j) & "Eurasia" %in% getRegions(j)) {
        j["Eurasia", , ] <- j["Eurasia", , ] - j["Russia", , ]
      }

      x.regional <- mbind(x.regional, .disaggregate_regions(x_in = j, regions_in = regions))
    }

    x.regional <- x.regional[, , "dummy", invert = TRUE]

    Non28EUcountries <- c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY")
    x.regional[Non28EUcountries, , ] <- 0

    return(x.regional)
  } else {
    stop("Not a valid subtype! Must be either \"region\" or \"global\"")
  }
}
