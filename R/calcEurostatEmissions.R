#' Calculate REMIND emission variables from historical Eurostat (env_air_gge) values
#'
#' @author Falk Benke
#'
calcEurostatEmissions <- function() {
  data <- readSource(type = "Eurostat", subtype = "latest")

  # set values for EU countries with no values to 0
  data <- toolFillEU34Countries(data)

  # convert N2O from Mt to kt (currently not done via the mapping)
  data[, , "N2O_native"] <- data[, , "N2O_native"] * 1000

  map <- toolGetMapping(name = "Mapping_EurostatCRF.csv", where = "mrremind", type = "reportingVariables") %>%
    filter(.data$REMIND != "") %>%
    mutate(
      from = paste0(.data$emi, ".", .data$sector),
      to = paste0(.data$REMIND, " (", .data$unit, ")")
    )

  x <- toolAggregate(data, rel = map, from = "from", to = "to", dim = 3, partrel = TRUE, verbosity = 2)
  getSets(x)[3] <- "variable"

  return(list(
    x = x, weight = NULL,
    unit = c("Mt CO2/yr", "Mt CH4/yr", "kt N2O/yr", "Mt CO2eq/yr"),
    description = "Historical Emission values from Eurostat (env_air_gge) as REMIND variables"
  ))
}
