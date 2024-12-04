#' Convert IIASA_subs_taxes data
#'
#' Convert IIASA subsidy and taxes data on ISO country level (removes countries
#' not part of 249 oficial ISO countries and fills missing with zeros).
#'
#'
#' @param x MAgPIE object containing IIASA subsidies and taxes data in country
#' resolution
#' @param subtype Type of country level data as compiled by IIASA that should
#' be read in. Available types are: \itemize{ \item \code{tax_rate}: tax rate
#' pre final energy category \item \code{subsidies_bulk}: subsidy rate per final
#' energy category \item \code{energy}: final energy quantities per category }
#' @return IIASA_subs_taxes data as MAgPIE object aggregated to country level
#' @author Christoph Bertram
#' @examples
#' \dontrun{
#' a <- convertIIASA_subs_taxes(x)
#' }
#'
convertIIASA_subs_taxes <- function(x, subtype) {

  x <- toolCountryFill(x,
    verbosity = 2, fill = 0,
    no_remove_warning = c("ADO", "CHI", "ZAR", "IMY", "KSV", "ROM", "TMP", "WBG")
  )

  # convert monetary data from $2005 to $2017
  if (subtype %in% c("subsidies_bulk", "tax_rate")) {
    x <- GDPuc::toolConvertGDP(
      gdp = x,
      unit_in = "constant 2005 US$MER",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )
  }

  return(x)
}
