#' write KLW damage parameters (from Kotz et al. 2024) into input data
#' they are country-specific and should not be aggregated to the regional level at all

#' @author Franziska Piontek
#' @param subtype "beta1", "beta2", "maxGMT"
#' @return MAgPIE object of damage parameters for KLW damage function on country level and for 1000 boostrapping samples

calcKLWdamage <- function(subtype) {
  if (subtype == "beta1") {
    output <- readSource("KLWdamage", subtype = "beta1")
    description <- "first damage coefficient for KLW damages (linear temperature)"
    # average weight
    weight <- new.magpie(getRegions(output), getYears(output), getNames(output), fill = 1)
  } else if (subtype == "beta2") {
    output <- readSource("KLWdamage", subtype = "beta2")
    description <- "second damage coefficient for KLW damages (temperature squared)"
    # average weight
    weight <- new.magpie(getRegions(output), getYears(output), getNames(output), fill = 1)
  } else if (subtype == "maxGMT") {
    output <- readSource("KLWdamage", subtype = "maxGMT")
    description <- "KLW damage: maximum GMT change for countries for which the KLW damage estimate is valid"
    # average weight
    weight <- new.magpie(getRegions(output), getYears(output), getNames(output), fill = 1)
  }
  return(list(x         = output,
              weight      = weight,
              unit        = "dimensionless",
              description = description))
}
