#' Converts BGR oil, gas, coal and uranium reserves data
#'
#' @param x MAgPIE object to be converted
#' @param subtype data subtype. Either "oil", "gas", "coal" or "uranium".
#' @return A MAgPIE object containing BGR (Federal Institute for Geosciences and
#' Natural Resources) country reserves disaggregated data of oil, gas, coal and uranium.
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' a <- convertBGR(x, subtype = "oil")
#' }
#'
convertBGR <- function(x, subtype) {
  # rename countries to REMIND iso codes
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  x[is.na(x)] <- 0 # setting NA values to zero
  x <- toolCountryFill(x, fill = 0, verbosity = 2) # fill countries with no data

  if (subtype == "coal") {
    # Historically, Turkey is able to provide most of its coal demand locally.
    # However, the BGR numbers used as disaggregation weight for ROW numbers in
    # REMIND11regi are so low in comparison to Canada (which is also part of ROW)
    # that Turkey gets too little coal resources in REMIND and therefore needs to
    # import most of its coal consumption, leading to extreme coal prices in 2010-2020
    # when import amounts are bounded. As a hotfix, we increase the resources
    # available to Turkey by a factor 10.

    # Source: this page here claims Turkey has 22 bn ton coal resource:
    # https://enerji.gov.tr/english-info-bank-natural-resources-coal),
    # and currently supplies >70% of its coal demand domestically
    x["TUR", , ] <- x["TUR", , ] * 10
  }

  return(x)
}
