#' Convert WGBU data
#'
#' Convert WGBU data on ISO country level.
#'
#'
#' @param x MAgPIE object containing WGBU data country-region resolution
#' @return WGBU data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' \dontrun{
#' a <- convertWGBU(x)
#' }
#'
convertWGBU <- function(x) {
  # remove world data
  # FIXME do we need it for the missing regions?
  x <- x["Welt", , invert = TRUE]

  # remove aggregated data ("total * ")
  # FIXME do we need it for the difference to the sum over the related countries?
  x <- x["Total", , pmatch = TRUE, invert = TRUE]

  # remove space from region names
  getItems(x, dim = 1) <- gsub(" ", "", getItems(x, dim = 1))
  # rename countries with ISO-code
  # FIXME delete Persien for now
  x <- x["Persien", , invert = TRUE]
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))

  # fill all missing countries with 0
  x <- toolCountryFill(x, fill = 0, verbose = 2)

  return(x)
}
