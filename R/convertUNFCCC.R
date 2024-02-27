#' Convert UNFCCC data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readUNFCCC()`].
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#'
#' @importFrom dplyr %>% filter
#' @importFrom madrat toolCountryFill toolGetMapping
#'
#' @export
convertUNFCCC <- function(x) {

  x <- toolCountryFill(x, verbosity = 2, no_remove_warning = "EUA")

  # fill countries of selected regions with 0 to allow for region aggregation
  regions.fill <- c("EUR", "REF", "NEU", "CAZ")
  mapping <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder") %>%
    filter(.data$RegionCode %in% regions.fill)

  tmp <- x[unique(mapping$CountryCode), , ]
  tmp[is.na(tmp)] <- 0
  x[unique(mapping$CountryCode), , ] <- tmp

  return(x)
}
