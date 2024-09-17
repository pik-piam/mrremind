#' Converts Openmod capacities data
#'
#' @param x MAgPIE object to be converted
#' @return A MAgPIE object containing openmod EU country disaggregated data with
#' 2010 and 2015 electricity capacities (GW)
#' @author Renato Rodrigues
#' @examples
#' \dontrun{
#' a <- convertOpenmod(x)
#' }
#'
#'

convertOpenmod <- function(x) {

  # loading LIMES country iso code
  LIMESMapping <- toolGetMapping("regionmappingLIMES.csv", where = "mappingfolder", type = "regional")

  # filter only relevant region names
  x <- x[LIMESMapping$LIMES_ISO2, , ]

  # rename countries to REMIND iso codes
  getItems(x, dim = 1) <- toolCountry2isocode(
    getItems(x, dim = 1),
    mapping = setNames(as.character(LIMESMapping$LIMES_ISO3), LIMESMapping$LIMES_ISO2)
  )

  # disaggregate BAL (Balkans) = BIH (Bosnia and Herzegovina), SRB (Serbia), MNE (Montenegro),
  # MKD (Macedonia, the former Yugoslav Republic of) and Albania (ALB)

  # fill countries with no data
  x <- toolCountryFill(x, fill = 0, verbosity = 2, no_remove_warning = "BAL")

  # replace NAs with 0
  x[is.na(x)] <- 0

  return(x)
}
