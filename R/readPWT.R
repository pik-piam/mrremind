#' Read PWT
#'
#' Read-in PWT data (version 8.0) as magclass object
#'
#' @order 1
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("PWT")
#' }
readPWT <- function() {
  pwt <- readxl::read_excel("pwt80.xlsx", sheet = "Data")
  # Remove "country", "currency_unit" and indicator ("i_") columns
  pwt <- pwt[, !grepl("(^country$|^currency_unit$|^i_)", names(pwt))]
  # Transform to magpie
  as.magpie(pwt)
}

#' @rdname readPWT
#' @order 2
#' @param x MAgPIE object returned by readPWT
convertPWT <- function(x) {
  getSets(x) <- c("iso3c", "year", "variable")
  x <- toolCountryFill(x)
  x[is.na(x)] <- 0
  x
}
