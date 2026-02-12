#' convert IIASA Land Use Emissions
#'
#' @author Falk Benke
#' @param x a magpie object
#' @inheritParams readIIASALanduse
#'
convertIIASALanduse <- function(x, subtype) {
  if (subtype %in% c("historical", "forecast2030")) {
    x <- x["European Union 27", , , invert = TRUE]
    getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1),
      mapping = c(
        "Netherlands, Kingdom of the" = "NLD",
        "Korea (Democratic People's Republic of)" = "PRK"
      )
    )
  } else if (subtype == "forecast2035") {
    x <- x[c("ANT", "EU"), , , invert = TRUE]
  }

  x <- toolCountryFill(x, verbosity = 2)

  return(x)
}
