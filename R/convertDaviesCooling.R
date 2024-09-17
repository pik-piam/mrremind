#' Convert Davies Cooling
#'
#' Convert Davies (2013) data on on shares of cooling types using mapping from
#' GCAM regions to ISO country level.
#'
#'
#' @param x MAgPIE object containing DaviesCooling data region resolution
#' @return MAgPIE object of the Davies (2013) data disaggregated to country
#' level
#' @author Lavinia Baumstark, Ioanna Mouratiadou
#' @seealso \code{\link{readDaviesCooling}}
#' @examples
#'
#' \dontrun{ a <- convertDaviesCooling(x)
#' }
#'
convertDaviesCooling <- function(x) {

  mapping <- "RegionMappingDavies2ISO.csv"
  y <- toolAggregate(x, mapping, weight=NULL )
  y <- toolCountryFill(y, fill = 0, verbosity = 2)

  return(y)
}
