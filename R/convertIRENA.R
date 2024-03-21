#' Converts IRENA Regional data
#'
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "Capacity" or "Generation"
#' @return A MAgPIE object containing IRENA country disaggregated data  with
#' historical electricity renewable capacities (MW) or generation levels (GWh)
#' @author Renato Rodrigues
#' @importFrom madrat toolCountry2isocode
#' @examples
#' \dontrun{
#' a <- convertIRENA(x, subtype = "Capacity")
#' }
#'
convertIRENA <- function(x, subtype) {
  # rename countries to REMIND iso codes
  getItems(x, dim = 1) <- gsub("\\*", "", getItems(x, dim = 1))
  getItems(x, dim = 1) <- gsub("South Georgia",
                               "South Georgia and the South Sandwich Islands",
                               getItems(x, dim = 1), fixed = TRUE)
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x[is.na(x)] <- 0
  # aggregate Kosovo to Serbia
  x1 <- x["KOS", , ]
  getItems(x1, dim = 1) <- c("SRB")
  x["SRB", , ] <- x["SRB", , ] + x1
  x <- x[c("KOS"), , , invert = TRUE]
  # fill countries with no data
  x <- toolCountryFill(x, fill = 0, verbosity = 2)
  return(x)
}
