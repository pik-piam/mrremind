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
    weight=NULL,
    unit = "fraction",
    description = "Share in global EEZ area. Source: marineregions.org"
  ))
}
