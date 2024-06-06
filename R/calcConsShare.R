#' Calculate the consumption shares for each of the 10 deciles across different regions
#'
#' This function calculates the consumption shares for sub-regional deciles within total consumption
#' across REMIND regions. It assumes a log-normal distribution for sub-regional consumption.
#' The calculation proceeds in three steps:
#' 1. Calculate 'sigma' (the variance of the distribution) from regional Theil index.
#' 2. Generate regional distribution with 'sigma', and integrate and normalize to compute
#' the relative share of each decile group.
#' 3. Assign regional values to individual countries within each region and set the
#' aggregation rule for calcOutput to calculate the average.
#'
#' @return magpie objects of regional consumption shares for each of the 10 deciles.
#' weights = 1, such that the aggregation later takes country average
#' @author Jiarui Zhong
#' @seealso \code{\link{calcOutput}} \code{\link{convertGini},\link{readGini}.\link{calcTheil}}
#' @examples
#' \dontrun{
#' a <- calcOutput("ConsShare")
#' }
#'

calcConsShare <- function() {

  # Get regional Theil index, regional mapping in getConfig() is used.
  theilT <- calcOutput(type = "Theil", aggregate = TRUE)

  # Calculate regional sigma from Theil
  sigma <- sqrt(2 * theilT)

  # helper function compute 10 income group share given sigma
  decShareFromSigma <- function(sigma) {
    # assume any value of mu, which doesn't affect the result
    mu <- 1

    # decile boundaries
    deciles <- stats::qlnorm(seq(0, 1, by = 0.1), meanlog = mu, sdlog = sigma)

    # function for the integrand x * f(x)
    integrand <- function(x) {
      x * stats::dlnorm(x, meanlog = mu, sdlog = sigma)
    }

    # Compute shares by integrating over each decile range
    incomeShares <- numeric(length(deciles) - 1)
    for (i in 2:length(deciles)) {
      incomeShares[i - 1] <- stats::integrate(integrand, lower = deciles[i - 1], upper = deciles[i])$value
    }

    # Normalize
    normalizedShares <- incomeShares / sum(incomeShares)

    return(normalizedShares)
  }

  consShare <- array(NA, dim = c(dim(sigma), 10),
    dimnames = c(dimnames(sigma), list("decile" = 1:10)))

  # Applying function across each Theil
  for (i in seq_len(dim(sigma)[1])) {
    for (j in seq_len(dim(sigma)[2])) {
      for (k in seq_len(dim(sigma)[3])) {
        consShare[i, j, k, ] <- decShareFromSigma(sigma[i, j, k])
      }
    }
  }

  consShare <- as.magpie(consShare)

  # assign regional value to sub-regional countries
  mapping <- toolGetMapping(
    type = "regional", name = getConfig()$regionmapping,
    returnPathOnly = FALSE, where = "mappingfolder"
  )

  consShareCountry <- toolAggregate(consShare, mapping, from = "RegionCode", to = "CountryCode")

  weight <- consShareCountry
  weight[ , , ] <- 1

  return(list(
    x = consShareCountry,
    weight = weight,
    unit = "Dimentionless",
    description = "Consumption share of 10 sub-regional income deciles for Remind regions,
    assigned to subregional countries"))
}
