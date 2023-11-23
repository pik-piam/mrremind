#' @title convertStegmann2022
#' @description Converts data from Stegmann2022
#' @param x unconverted magpie object from read-script
#'
#' @return magpie object with a completed dataset.
#'

convertStegmann2022 <- function(x) {
  x <- x[c("World"), , , invert = TRUE]

  regmapping <- toolGetMapping("regionmapping_IMAGE_PBL_Stegmann2022.csv", where = "mrremind", type = "regional")

  fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)[unique(regmapping$CountryCode), 2016, "FE (EJ/yr)"]

  out <- toolAggregate(x, regmapping, from = "RegionAbbreviation", to = "CountryCode", weight = fe)
  out <- toolCountryFill(out, fill = 0)

  return(out)
}
