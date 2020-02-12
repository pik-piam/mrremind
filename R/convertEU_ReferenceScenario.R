#' Convert EU Reference Scenario
#'
#' Converts EU Reference Scenario magpie object into appropriate form for the REMIND model
#' 
#' @param x EU Reference Scenario magpie object derived from readEU_ReferenceScenario function
#' @return converted EU Reference Scenario magpie object
#' @author Renato Rodrigues
#' @source EU Reference Scenario public database http://data.europa.eu/euodp/en/data/dataset/energy-modelling
#' @examples
#' \dontrun{test <- readSource("EU_ReferenceScenario",convert=TRUE)}

convertEU_ReferenceScenario <- function(x){
  iso3 <- read.csv2("isotwo2iso3Mapping.csv",stringsAsFactors = FALSE)
  getRegions(x) <- sapply(getRegions(x), function(y) iso3[which(iso3[,1] == y),2])
  # fill up zero countries
  x <- toolCountryFill(x)
  return(x)
}