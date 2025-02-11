#' Read IIASA subsidies and taxes
#'
#' Read-in country level data on final energy taxes and subsidies as provided
#' from IIASA from .csv file as magclass object
#'
#'
#' @param subtype Type of country level data as compiled by IIASA that should
#' be read in. Available types are: \itemize{ \item \code{tax_rate}: tax rate
#' pre final energy category \item \code{subsidies_bulk}: subsidy rate per final
#' energy category \item \code{energy}: final energy quantities per category }
#' @return magpie object of the IIASA_subs_taxes data
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "IIASA_subs_taxes", "tax_rate")
#' }
#'
readIIASA_subs_taxes <- function(subtype) {

  file <- "unlinked_countries_2017_03.xlsx"

  if (!subtype %in% c("tax_rate", "subsidies_bulk", "energy")) {
    stop(paste('Invalid subtype -- supported subtypes are:',
               paste(c("tax_rate", "subsidies_bulk", "energy"), collapse = ', ')))
  }

  data <- read_excel(file, sheet = subtype)
  data <- data[!is.na(data[[1]]), ]

  data$CountryName <- NULL # remove country
  x <- as.data.frame(sapply(data[!names(data) == "CountryCode"], as.numeric))
  x$CountryCode <- data$CountryCode
  as.magpie(x)
}

#' Convert IIASA_subs_taxes data
#'
#' Convert IIASA subsidy and taxes data on ISO country level (removes countries
#' not part of 249 oficial ISO countries and fills missing with zeros).
#'
#' @param x MAgPIE object containing IIASA subsidies and taxes data in country
#' resolution
#' @param subtype Type of country level data as compiled by IIASA that should
#' be read in. Available types are: \itemize{ \item \code{tax_rate}: tax rate
#' pre final energy category \item \code{subsidies_bulk}: subsidy rate per final
#' energy category \item \code{energy}: final energy quantities per category }
#' @return IIASA_subs_taxes data as MAgPIE object aggregated to country level
#' @examples
#' \dontrun{
#' a <- convertIIASA_subs_taxes(x)
#' }
#'
convertIIASA_subs_taxes <- function(x, subtype) {

  x <- toolCountryFill(x,
                       verbosity = 2,
                       fill = 0,
                       no_remove_warning = c("ADO", "CHI", "ZAR", "IMY", "KSV", "ROM", "TMP", "WBG"))

  # convert monetary data from $2005 to $2017
  if (subtype %in% c("subsidies_bulk", "tax_rate")) {
    x <- GDPuc::toolConvertGDP(
      gdp = x,
      unit_in = "constant 2005 US$MER",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )
  }

  x
}
