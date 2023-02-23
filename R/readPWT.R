#' Read PWT
#'
#' Read and convert PWT data
#
#' @family "Past" GDPpc functions
#' @return Magpie object of the PWT data
readPWT <- function() {
  pwt <- readxl::read_excel("pwt80.xlsx", sheet = "Data")
  # Remove "country", "currency_unit" and indicator ("i_") columns
  pwt <- pwt[, !grepl("(^country$|^currency_unit$|^i_)", names(pwt))]
  # Transform to magpie
  as.magpie(pwt)
}

#' @describeIn readPWT Convert PWT data
#' @param x MAgPIE object returned by readPWT
convertPWT <- function(x) {
  # Remove any NA-countries
  x <- x[!is.na(getCells(x)), , ]

  # Remove years which only contain NAs
  x <- x[, !apply(x, 2, function(y) all(is.na(y))), ]

  # Use default setNames
  getSets(x) <- c("iso3c", "year", "variable")

  # Substitute NAs
  x[is.na(x)] <- 0

  x <- suppressMessages(toolCountryFill(x, fill = 0))

  # Sort by year
  x <- x[, sort(getYears(x)), ]
}
