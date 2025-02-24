#' Read Gini
#'
#' Read Gini coefficients for SSP scenarios from Rao et al., Futures, 2018. Data has been provided by the authors, but
#' will be made publicly available as well. This contains data for 184 countries and from 2011 onwards.
#'
#' Copied from the documentation provided by the authors:
#' This sheet contains the original Gini projections for 43 countries from the underlying empirical model
#' (See reference to RSP 2016 in the main paper) and the extrapolations to all countries using the methodology
#' described in the article. The country codes are the World Bank codes.
#'
#' @return magpie object of the Gini data
#' @seealso \code{\link{readSource}}
#' @order 1
#' @examples
#' \dontrun{
#' a <- readSource(type="Gini")
#' }
readGini <- function() {
  as.magpie(readxl::read_excel("Gini_projections_SSPs.xlsx", sheet = 2))
}

#' @rdname readGini
#' @order 2
#' @param x MAgPIE object returned from readGini
convertGini <- function(x) {
  getSets(x)[1] <- "iso3c"

  # Fill missing countries with a small number (not zero because this might cause numerical problems in Gini
  # aggregation later)
  x <- toolCountryFill(x, fill = 1e-8, verbosity = 2)

  # Convert to 0..1 range
  x / 100
}
