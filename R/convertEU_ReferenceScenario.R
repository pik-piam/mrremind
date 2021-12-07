#' Convert EU Reference Scenario
#'
#' Converts EU Reference Scenario magpie object into appropriate form for the REMIND model
#'
#' @param x EU Reference Scenario magpie object derived from readEU_ReferenceScenario function
#' @return converted EU Reference Scenario magpie object
#' @author Renato Rodrigues, Falk Benke
#' @param subtype data subtype. Either "2016" or "2020"
#' @examples
#' \dontrun{
#' test <- readSource("EU_ReferenceScenario", subtype = "2020", convert = TRUE)
#' }
#'
convertEU_ReferenceScenario <- function(x, subtype) {

  # fill up zero countries
  iso3 <- read.csv2("isotwo2iso3Mapping.csv", stringsAsFactors = FALSE)
  getItems(x, dim = 1) <- sapply(getItems(x, dim = 1), function(y) iso3[which(iso3[, 1] == y), 2])
  
  x <- toolCountryFill(x, fill = NA, verbosity = 2)

  EU_27 <- c(
    "ALA", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
    "FRO", "FIN", "FRA", "DEU", "GIB", "GRC", "GGY", "HUN", "IRL",
    "IMN", "ITA", "JEY", "LVA", "LTU", "LUX", "MLT", "NLD", "POL",
    "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
  )

  if (subtype == "2016") {
    EU <- c(EU_27, "GBR")
  } else {
    EU <- EU_27
  }

  x.eu <- x[EU,,]
  x.eu[is.na(x.eu)] <- 0
  x[EU, , ] <- x.eu[EU, , ]
  x <- add_dimension(x, dim = 3.1, add = "model", nm = paste0("EU_ReferenceScenario_", subtype))

  return(x)
}
