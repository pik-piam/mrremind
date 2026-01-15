#' Convert IEA ETP projections
#'
#' @author Falk Benke, Robin Hasse
#'
#' @param x IEA ETP projection magpie object derived from readIEA_ETP function
#' @param subtype data subtype. Either "industry", "buildings", "summary", or "transport"
#'
#'
convertIEA_ETP <- function(x, subtype) { # nolint: object_name_linter.

  # FUNCTIONS ------------------------------------------------------------------

  # filter for country regions and rename to ISO code
  getCountryRegions <- function(x) {
    countries <- toolCountry2isocode(getItems(x, 1), warn = FALSE)
    getItems(x, 1) <- countries
    x[!is.na(countries), , ]
  }

  # list known and unkown countries in given region
  getCountries <- function(region, map, xDisagg) {
    countries <- if (region == "WORLD") {
      map[["CountryCode"]]
    } else {
      map[map[[region]], "CountryCode"]
    }
    list(
      known = intersect(countries, getItems(xDisagg, 1)),
      unknown = setdiff(countries, getItems(xDisagg, 1))
    )
  }

  # aggregate quantity from known regions
  getKnownQuantity <- function(xDisagg, countries, region) {
    if (length(countries) == 0) return(0)
    xRegKnown <- dimSums(xDisagg[countries, , ], 1, na.rm = TRUE)
    getItems(xRegKnown, 1) <- region
    return(xRegKnown)
  }

  # disaggregate given region and add it to already disaggregated data
  addDisagg <- function(x, xDisagg, region, map, weight) {
    countries <- getCountries(region, map, xDisagg)
    xRegKnown <- getKnownQuantity(xDisagg, countries$known, region)
    xReg <- x[region, , ] - xRegKnown
    weightReg <- weight[countries$unknown, , ]
    mapReg <- data.frame(from = region, to = countries$unknown)
    xRegDisagg <- toolAggregate(xReg, mapReg, weightReg)
    mbind(xDisagg, xRegDisagg)
  }



  # READ -----------------------------------------------------------------------

  map <- toolGetMapping("regionmappingIEA_ETP.csv",
                        where = "mrremind", type = "regional",
                        returnPathOnly = TRUE) %>%
    utils::read.csv2(check.names = FALSE)

  fe <- calcOutput("FE", aggregate = FALSE) %>%
    mselect(d3 = switch(subtype,
                        industry  = "FE|Industry (EJ/yr)",
                        buildings = "FE|Buildings (EJ/yr)",
                        transport = "FE|Transport (EJ/yr)",
                        summary   = "FE (EJ/yr)")) %>%
    collapseDim() %>%
    time_interpolate(getItems(x, 2), extrapolation_type = "constant")



  # DISAGGREGATE ---------------------------------------------------------------

  xDisagg <- getCountryRegions(x)

  for (region in c("ASEAN", "European Union", "OECD", "WORLD")) {
    xDisagg <- addDisagg(x, xDisagg, region, map, fe)
  }

  return(xDisagg)
}
