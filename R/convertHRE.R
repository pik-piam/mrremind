#' Convert HRE data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readHRE()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
convertHRE <- function(x) {

  # map scenario names
  scen_map <- c(BL="HRE_Baseline", CD="HRE_ConvDecarb", HRE="HRE_HeatRoadmap")
  getNames(x, dim=1) <- scen_map[getNames(x, dim=1)]

  # remove duplicate entries (with different DataType)
  x <- x[, , "Output.Installed capacity|Heating|DH - Waste incineration.MWth", invert=T]

  # remove DataType and Unit column
  x <- collapseDim(x, dim=c("DataType", "Unit"))

  # add missing countries
  x <- toolCountryFill(x, fill = NA, verbosity = 2)

  return(x)
}
