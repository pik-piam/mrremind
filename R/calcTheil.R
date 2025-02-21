#' Get regional Theil-T index projections
#'
#' To calculate the regional Theil-T index (= correction to welfare function for a lognormal income distribution) we do
#' the following: (1) convert country-level Gini coefficients to Theil (2) calculate contribution to Theil-T index that
#' includes both between-countries and within-country inequality (see e.g. https://en.wikipedia.org/wiki/Theil_index).
#' The latter can then be aggregated with calcOutput().
#'
#' The projections are SSP specific and use SSP GINI projections. For non-SSP scenarios, the projections are equal
#' to the SSP2 projection.
#'
#' The aggregation depends on the region mapping. It is implemented such
#' that the regionmapping specified in getConfig()$regionmapping is used.
#' The result of calcOutput('Theil', aggregate = FALSE), is NOT the country
#' Theil-T, but the unweighted contribution from a given country to the regional value.
#'
#' @return magpie objects of unweighted contribution to Theil, weights (= country shares of regional GDP)
#' @param scenario A string (or vector of strings) designating the scenario(s) to be returned. Passed as is
#' to the scenario argument of [mrdrivers::calcGDP].
#' @seealso \code{\link{calcOutput}}, \code{\link{readGini}}
#'
calcTheil <- function(scenario) {
  # Get SSP Gini projections
  Gini <- readSource("Gini")
  ## The original data range is 2011-2100 in one-year steps, here we extend it to 2000-2150 in 5-year steps.
  ## Values before (after) the original range are held fixed at 2011 (2100) levels.
  ## (time_interpolate breaks set names, so restore them explicitly)
  xsets <- getSets(Gini)
  Gini <- time_interpolate(Gini,
                           interpolated_year = c(seq(2000, 2010, by = 5), seq(2105, 2150, by = 5)),
                           integrate_interpolated_years = TRUE,
                           extrapolation_type = "constant")
  Gini <- Gini[, seq(2000, 2150, by = 5), ]
  getSets(Gini) <- xsets

  # Convert Gini to sigmas assuming lognormal distribution
  sigma <- sqrt(2) * stats::qnorm((Gini + 1) / 2)
  # Theil T coefficient for lognormal distribution
  TheilT <- sigma^2 / 2

  # We need the GDP and GDP per capita scenarios, for the scenarios and years of Gini.
  # We set extension2150 = "constant" because the Gini coefficients are also extended in the same way.
  y <- getYears(TheilT)
  gdp      <- calcOutput("GDP", scenario = scenario, extension2150 = "constant", years = y, aggregate = FALSE)
  gdpReg   <- calcOutput("GDP", scenario = scenario, extension2150 = "constant", years = y)
  gdppc    <- calcOutput("GDPpc", scenario = scenario, extension2150 = "constant", years = y, aggregate = FALSE)
  gdppcReg <- calcOutput("GDPpc", scenario = scenario, extension2150 = "constant", years = y)

  # If required, add any non SSP scenarios. By default use the SSP2 TheilT scenario.
  if (any(! getNames(gdp) %in% getNames(TheilT))) {
    scens <- getNames(gdp)[! getNames(gdp) %in% getNames(TheilT)]
    message(glue::glue("Adding {paste(scens, collapse = ', ')} as copies of SSP2."))
    addTheilT <- purrr::map(scens, ~setNames(TheilT[, , "SSP2"], .x)) %>% mbind()
    TheilT <- mbind(TheilT, addTheilT)
  }

  # Keep only required scenarios
  TheilT <- TheilT[, , getNames(gdp)]

  # Allocate empty objects for storing Theil contribution and weights
  contribTheilT <- TheilT * NA
  weight <- TheilT * NA

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
