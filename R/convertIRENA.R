#' Converts IRENA Regional data
#'
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "Capacity" or "Generation"
#' @return A MAgPIE object containing IRENA country disaggregated data  with
#' historical electricity renewable capacities (MW) or generation levels (GWh)
#' @author Renato Rodrigues, Pascal Weigmann
#' @examples
#' \dontrun{
#' a <- convertIRENA(x, subtype = "Capacity")
#' }
#'
convertIRENA <- function(x, subtype) {

  x[is.na(x)] <- 0

  # aggregate Kosovo to Serbia
  x1 <- x["XKX", , ]
  getItems(x1, dim = 1) <- c("SRB")
  x["SRB", , ] <- x["SRB", , ] + x1
  x <- x[c("XKX"), , , invert = TRUE]
  # fill countries with no data
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  return(x)
}