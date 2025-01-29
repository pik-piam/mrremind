#' Calculate regional Theil-T index
#'
#' To calculate the regional Theil-T index (= correction to welfare function for a lognormal income distribution) we do
#' the following: (1) convert country-level Gini coefficients to Theil (2) calculate contribution to Theil-T index that
#' includes both between-countries and within-country inequality (see e.g. https://en.wikipedia.org/wiki/Theil_index).
#' The latter can then be aggregated with calcOutput().
#'
#' NB 1: the aggregation depends on the region mapping. It is implemented such
#' that the regionmapping specified in getConfig()$regionmapping is used.
#'
#' NB 2: the result of calcOutput('Theil', aggregate = FALSE), is NOT the country
#' Theil-T, but the unweighted contribution from a given country to the regional value.
#'
#' @return magpie objects of unweighted contribution to Theil, weights (= country shares of regional GDP)
#' @author Bjoern Soergel
#' @seealso \code{\link{calcOutput}} \code{\link{convertGini},\link{readGini}}
#' @examples
#' \dontrun{
#'   calcOutput("Theil")
#' }
#'
calcTheil <- function() {
  # Read Gini
  Gini <- readSource("Gini")

  # Convert Gini to sigmas assuming lognormal distribution
  sigma <- sqrt(2) * stats::qnorm((Gini + 1) / 2)
  # Theil T coefficient for lognormal distribution
  TheilT <- sigma^2 / 2

  # We need the GDP and GDP per capita scenarios, for the scenarios and years of Gini.
  # We set extension2150 = "constant" because the Gini coefficients are also extended in the same way.
  # As a regionmapping we use the one set in the config (which is the default behavior). The same is called explicitly
  # later, as it is used in the calculations of the Theil contribution and weights.
  s <- getNames(Gini)
  y <- getYears(Gini)
  gdp      <- calcOutput("GDP",
                         scenario = c("SSPs", "SDPs"),
                         naming = "scenario",
                         extension2150 = "constant",
                         years = y,
                         aggregate = FALSE)[, , s]
  gdpReg   <- calcOutput("GDP",
                         scenario = c("SSPs", "SDPs"),
                         naming = "scenario",
                         extension2150 = "constant",
                         years = y)[, , s]
  gdppc    <- calcOutput("GDPpc",
                         scenario = c("SSPs", "SDPs"),
                         naming = "scenario",
                         extension2150 = "constant",
                         years = y,
                         aggregate = FALSE)[, , s]
  gdppcReg <- calcOutput("GDPpc",
                         scenario = c("SSPs", "SDPs"),
                         naming = "scenario",
                         extension2150 = "constant",
                         years = y)[, , s]

  # Allocate empty objects for storing Theil contribution and weights
  contribTheilT <- TheilT
  contribTheilT[, , ] <- NA
  weight <- TheilT
  weight[, , ] <- NA

  # Compute Theil contribution and weights
  regionmapping <- toolGetMapping(getConfig("regionmapping"), type = "regional", where = "mappingfolder")
  for (rr in getRegions(gdppcReg)) {
    rrCountries <- regionmapping$CountryCode[regionmapping$RegionCode == rr]
    # Contribution to Theil index (unweighted)
    contribTheilT[rrCountries, , ] <- TheilT[rrCountries, , ] + log(gdppc[rrCountries, , ] / gdppcReg[rr, , ])
    # Weights = country shares of regional GDP
    weight[rrCountries, , ] <- gdp[rrCountries, , ] / gdpReg[rr, , ]
    # Sanity check: ensure that weights for a region sum to one (within floating point precision)
    stopifnot(max(abs(dimSums(weight[rrCountries, , ], dim = 1) - 1)) < 1e-10)
  }

  list(x = contribTheilT,
       weight = weight,
       unit = "-",
       description = "Aggregated: Theil-T index. Not-aggregated: unweighted contribution to Theil-T")
}
