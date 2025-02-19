#' Calculate distribution of total EEZ size
#'
#' @author Tabea Dorndorf
#'
calcEEZdistribution <- function() {
  x <- readSource("MarineRegionsOrg")

  x[is.na(x)] <- 0

  # calculate the share of each country in the total area
  x <- x / sum(x)

  getNames(x) <- NULL

  return(list(
    x = x,
    weight = NULL,
    unit = "fraction",
    description = "Share in global EEZ area.
    Calculated based on area provided by marineregions.org.
    Note that EEZ areas can differ widely between some sources for certain
    countries. Factors include how joint regime areas and overlapping claims
    are handled, and the detail of the grid used to calculate 200 nm zone.
    marineregions.org is what also the other sources refer to. We use their
    calculation of area size for purpose of clarity."
  ))
}
