#' Convert NREL data
#'
#' Convert NREL data on ISO country level.
#'
#'
#' @param x MAgPIE object containing NREL data country-region resolution
#' @return NRELWirsenius data as MAgPIE object aggregated to country level
#' @author Lavinia Baumstark
#' @examples
#' \dontrun{
#' a <- convertNREL(x, subtype = "onshore")
#' }
convertNREL <- function(x) {
  # rename countries with ISO-code
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))

  # Allocation of aggregations
  # "ANT" -> "SXM", "CUW", "BES"
  # "YUG" -> "SRB", "MNE"  # YUG seems to be only these two, "SVN", "HRV", "MKD", "BIH"
  m <- matrix(c(
    c("ANT", "ANT", "ANT", "YUG", "YUG"), # ,"YUG","YUG","YUG","YUG"),
    c("SXM", "CUW", "BES", "SRB", "MNE")
  ), 5) # ,"SVN","HRV","MKD","BIH")),9)
  w <- calcOutput("Population", scenario = "SSP2", aggregate = FALSE)[m[, 2], 2010, ]
  x_split <- toolAggregate(x[c("ANT", "YUG"), , ], m, weight = w)
  # delete ANT and YUG from x
  x <- x[c("ANT", "YUG"), , invert = TRUE]
  x <- mbind(x, x_split)

  # fill all missing countries with 0
  toolCountryFill(x, fill = 0, verbosity = 2)
}
