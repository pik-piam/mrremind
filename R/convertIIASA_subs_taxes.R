#' Convert IIASA_subs_taxes data
#'
#' Convert IIASA subsidy and taxes data on ISO country level (removes countries
#' not part of 249 oficial ISO countries and fills missing with zeros).
#'
#'
#' @param x MAgPIE object containing IIASA subsidies and taxes data in country
#' resolution
#' @return IIASA_subs_taxes data as MAgPIE object aggregated to country level
#' @author Christoph Bertram
#' @examples
#' \dontrun{
#' a <- convertIIASA_subs_taxes(x)
#' }
#'
convertIIASA_subs_taxes <- function(x) {

  x <- toolCountryFill(x,
    verbosity = 2, fill = 0,
    no_remove_warning = c("ADO", "CHI", "ZAR", "IMY", "KSV", "ROM", "TMP", "WBG")
  )

  return(x)
}
