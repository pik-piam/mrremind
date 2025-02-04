#' Read IEA Energy Prices
#'
#' Read-in an IEA Energy Prices file as magpie object
#'
#' @author Falk Benke
#' @importFrom data.table
#' @importFrom madrat as.magpie
readIEA_EnergyPrices <- function() {
  data <- data.table::fread(
    file = file.path("2022", "EPT_prices_USD.TXT"),
    col.names = c("SECTOR", "COUNTRY", "PRODUCT", "UNIT", "TIME", "value"),
    colClasses = c("character", "character", "character", "character", "character", "numeric"),
    sep = " ", stringsAsFactors = FALSE, na.strings = c("x", "..", "c"), skip = 0, showProgress = FALSE
  )

  return(as.magpie(data, spatial = 2, temporal = 5))
}
