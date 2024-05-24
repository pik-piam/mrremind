#' Reads shares of world manufacture for spv modules and wind turbines.
#' @author Aman Malik
#' @param x input magpie object
#' @return magpie object with shares

convertProdShares <- function(x) {
  x <- readSource(type = "ProdShares", convert = F)

  # mapping needed for disaggregation
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")

  # weight needed for disaggregation
  wt <- calcOutput(type = "GDP", aggregate = F)

  # Disaggregating "Europe" in spv
  mapping_eur <- mapping[mapping$RegionCode == "EUR", ]
  mapping_eur$RegionCode <- "Europe"

  wt_eur <- wt[mapping_eur$CountryCode, 2020, "gdp_SSP2"]
  x2 <- toolAggregate(x = x["Europe", "y2018", "spv"], rel = mapping_eur, weight = wt_eur, from = "CountryCode", to = "RegionCode")

  # Disaggregating "Other" in spv
  spv <- x[, "y2018", "spv"]
  other_cont <- data.frame(setdiff(mapping[mapping$RegionCode != "EUR", ]$CountryCode, toolCountry2isocode(getRegions(spv[which(spv > 0)]))))
  other_cont$Region <- "Other"
  colnames(other_cont)[1] <- "Country"
  wt_other <- wt[other_cont$Country, "y2020", "gdp_SSP2"]
  x3 <- toolAggregate(x = spv["Other", , ], rel = other_cont, from = "Country", to = "Region", weight = wt_other)

  # Disaggregating "Other" in wind

  wind <- x[, , "wind"]
  wind["Other", , ] <- 1 - dimSums(wind, dim = 1, na.rm = T)

  other_cont <- data.frame(setdiff(mapping$CountryCode, toolCountry2isocode(getRegions(wind[which(wind > 0)]))))
  other_cont$Region <- "Other"
  colnames(other_cont)[1] <- "Country"
  wt_other <- wt[other_cont$Country, "y2020", "gdp_SSP2"]
  x4 <- toolAggregate(x = wind["Other", , ], rel = other_cont, from = "Country", to = "Region", weight = wt_other)

  # creating copy of x without aggregated regions and countries as ISO and not NA
  y <- x[c("Europe", "Other"), , invert = T]
  getRegions(y) <- toolCountry2isocode(getRegions(y))
  y[, 2019, "spv"] <- y[, 2018, "spv"] # forcing spv shares in 2019 to be same as 2018
  y[is.na(y)] <- 0

  # creating new magpie with disaggregated values
  z <- new.magpie(mapping$CountryCode, years = getYears(x), names = getNames(x))
  z[getRegions(x2), , "spv"] <- x2 # adding elements from disaggregated spv
  z[getRegions(x3), , "spv"] <- x3
  z[getRegions(x4), , "wind"] <- x4 # adding elements from disaggegrated wind

  # adding elements orignially in magpie object
  for (i in getRegions(y)) {
    for (j in getNames(y)) {
      for (k in getYears(y)) {
        if (y[i, k, j] != 0) {
          z[i, k, j] <- y[i, k, j]
        }
      }
    }
  }

  x <- toolCountryFill(z, fill = 0, verbosity = 2)

  return(x)
}
