#' Converts Eurostat renewable energy share data
#'
#' @param x MAgPIE object to be converted
#' @param subtype data subtype
#' @return A MAgPIE object containing Eurostat renewable energy shares
#' @author Felix Schreyer
#'
convertEurostat_REShare <- function(x, subtype) {
  # Fill missing countries with NA so the output matches full ISO3 coverage
  ReShareFilled <- toolCountryFill(x, fill = NA, verbosity = 2)
  return(ReShareFilled)
}
