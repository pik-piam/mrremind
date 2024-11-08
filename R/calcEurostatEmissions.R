#' Calculate REMIND emission variables from historical Eurostat (env_air_gge) values
#'
#' @md
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Falk Benke
#' @export
calcEurostatEmissions <- function() {

  eurostatEmi <- readSource(type = "Eurostat", subtype = "latest")

  # set values for EU countries with no values to 0
  eurostatEmi <- toolFillEU34Countries(eurostatEmi)

  map <- toolGetMapping(name = "Mapping_EurostatCRF.csv", where = "mrremind", type = "reportingVariables") %>%
    filter(.data$REMIND != "") %>%
    mutate(from = paste0(.data$emi, ".", .data$sector),
           to = paste0(.data$REMIND, " (", .data$unit, ")"))

  x <- toolAggregate(eurostatEmi, rel = map, from = "from", to = "to", dim = 3, partrel = TRUE, verbosity = 2)

  return(list(
    x = x, weight = NULL,
    unit = c("Mt CO2/yr", "Mt CH4/yr", "kt N2O/yr", "Mt CO2eq/yr"),
    description = "Historical Emission values from Eurostat (env_air_gge) as REMIND variables"
  ))
}
