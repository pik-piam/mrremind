#' Convert ClimateTrace data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readClimateTrace()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Pascal Weigmann
#'
#' @export

convertClimateTrace <- function(x) {

  # aggregate Kosovo to Serbia and Northern Cyprus to Cyprus
  x1 <- x["XKX", , ]
  x1[is.na(x1)] <- 0
  getItems(x1, dim = 1) <- c("SRB")
  x["SRB", , ] <- x["SRB", , ] + x1

  x1 <- x["ZNC", , ]
  x1[is.na(x1)] <- 0
  getItems(x1, dim = 1) <- c("CYP")
  x["CYP", , ] <- x["CYP", , ] + x1

  # also remove "UNK" = Unknown
  x <- x[c("XKX", "ZNC", "UNK"), , , invert = TRUE]

  # add missing countries
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  # replace NA by 0 to enable aggregation over incomplete regions
  x[is.na(x)] <- 0

  return(x)
}
