#' Convert European Energy Datasheets
#'
#' @param x European Energy Datasheets magpie object derived from readEuropeanEnergyDatasheets function
#' @param subtype data subtype. Either "EU28" (data from June 20 including GBR)
#' or "EU27" (latest data from August 23 without GBR)
#' @return converted European Energy Datasheets magpie object
#' @author Renato Rodrigues and Atreya Shankar
#' @source European Energy Datasheets public database
#' https://energy.ec.europa.eu/data-and-analysis/eu-energy-statistical-pocketbook-and-country-datasheets_en
#' @examples
#' \dontrun{
#' test <- readSource("EuropeanEnergyDatasheets", subtype = "EU27", convert = TRUE)
#' }
#'
convertEuropeanEnergyDatasheets <- function(x, subtype) {
  iso3 <- read.csv2("isotwo2iso3Mapping.csv", stringsAsFactors = FALSE)
  getItems(x, dim = 1) <- sapply(getRegions(x), function(y) iso3[which(iso3[, 1] == y), 2])

  # fill up zero countries
  x <- toolCountryFill(x, fill = NA, verbosity = 2)

  # fill smaller EU-countries with 0s to allow for aggregation of EU-region
  x[c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY"), , ] <- 0

  # in never mapping, this is handled directly in mapping
  if (subtype == "EU28") {
    # removing international aviation from total emissions (REMIND reports total emissions without bunkers by default)
    x[, , "Emi|CO2 (Mt CO2/yr)"] <- x[, , "Emi|CO2 (Mt CO2/yr)"] -
      x[, , "Emi|CO2|Transport|Pass|Aviation|International|Demand (Mt CO2/yr)"]
    x[, , "Emi|GHG (Mt CO2eq/yr)"] <- x[, , "Emi|GHG (Mt CO2eq/yr)"] -
      x[, , "Emi|GHG|Bunkers|International Aviation (Mt CO2eq/yr)"]
  }
  return(x)
}
