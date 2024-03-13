#' Convert Gini
#'
#' Converts Gini data from readGini() to ISO country level.
#' Countries missing in the original data set will have their Gini set to zero (
#' a very small number for numerical reasons to be precise).
#' The original data range is 2011-2100 in one-year steps,
#' here we extend it to 2000-2150 in 5-year steps.
#' Values before (after) the original range are held fixed at 2011 (2100) levels.
#' Gini values for the SDP scenario are taken from the SSP1 scenario
#'
#'
#' @param x MAgPIE object containing Gini data with World Bank codes, 2011-2100, in percent (range 0-100)
#' @return MAgPIE object of the Gini data in ISO countries, range 0-1
#' @author Bjoern Soergel
#' @seealso \code{\link{readSource}} \code{\link{readGini}}
#' @examples
#' \dontrun{
#' a <- convertGini(x)
#' }
#' @importFrom countrycode countrycode


convertGini <- function(x) {
  # add SDP scenario, with same data as SSP1
  xSDP <- x[, , "SSP1"]
  getNames(xSDP) <- "SDP"
  x <- mbind(x, xSDP)

  # add SSP2EU, same data as SSP2
  xSSP2EU <- x[, , "SSP2"]
  getNames(xSSP2EU) <- "SSP2EU"
  x <- mbind(x, xSSP2EU)

  # conversion to iso3c codes, match Somalia by hand because countrycode fails
  getItems(x, dim = 1) <- countrycode(getRegions(x), origin = "wb",
                                      destination = "iso3c", custom_match = c("SOM" = "SOM"))
  getSets(x)[1] <- "iso3c"

  # fill missing countries with a small number (not zero because this might cause numerical problems in Gini aggregation later)
  x <- toolCountryFill(x, fill = 1e-8, verbosity = 2)

  # fill years outside original range
  # NB: time_interpolate breaks set names, so restore them explicitly
  xsets <- getSets(x)
  x <- time_interpolate(x, interpolated_year = c(seq(2000, 2010, by = 5), seq(2105, 2150, by = 5)),
                        integrate_interpolated_years = TRUE, extrapolation_type = "constant")
  x <- x[, seq(2000, 2150, by = 5), ]
  getSets(x) <- xsets

  # convert to 0..1 range
  x <- x / 100

  return(x)
}
