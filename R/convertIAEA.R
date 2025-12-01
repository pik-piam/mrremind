#' Nuclear data from world-nuclear.org
#'
#' @description  Data on currently operating and under-construction nuclear power
#' plants, reactors planned and proposed, electricity generation from nuclear
#' @author Christoph Bertram
#' @param x MAgPIE object to be converted

convertIAEA <- function(x) {
  # remove world data
  x <- x["WORLD", , , invert = TRUE]

  # rename countries into ISO
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  # fill missing countries
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  x[is.na(x)] <- 0

  return(x)
}
