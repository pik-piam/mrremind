#' Convert RemindCesPrices
#'
#' Converts CES derivatives/prices from former REMIND runs to ISO level
#'
#'
#' @return magpie object of REMIND prices
#' @author Antoine Levesque
#' @seealso \code{\link{readSource}}
#' @param x MAgPIE object containing REMIND prices at the REMIND region resolution
#' @param subtype Regional resolution of REMIND data which should be loaded. ccd632d33a corresponds to the REMIND-11,
#'  and 690d3718e1 to REMIND-H12
#'
#'
convertRemindCesPrices <- function(x, subtype = "ccd632d33a") {
  if (subtype == "ccd632d33a") {
    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv", returnPathOnly = TRUE, where = "mappingfolder")
  } else if (subtype == "690d3718e1") {
    mappingfile <- toolGetMapping(type = "regional", name = "regionmappingH12.csv", returnPathOnly = TRUE, where = "mappingfolder")
  } else {
    stop("valid subtypes are 'ccd632d33a', '690d3718e1'")
  }

  x <- toolAggregate(x, mappingfile, weight = NULL)

  return(x)
}
