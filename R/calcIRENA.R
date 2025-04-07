#' Calculate REMIND variables from historical IRENA capacities.
#'
#' @author Falk Benke
#' @export
calcIRENA <- function() {
  mapping <- tibble::tribble(
    ~irena,                     ~mif,
    "Geothermal",               "Cap|Electricity|Geothermal (GW)",
    "Renewable hydropower",     "Cap|Electricity|Hydro (GW)",
    "Wind",                     "Cap|Electricity|Wind (GW)",
    "Onshore wind energy",      "Cap|Electricity|Wind|Onshore (GW)",
    "Offshore wind energy",     "Cap|Electricity|Wind|Offshore (GW)",
    "Solar",                    "Cap|Electricity|Solar (GW)",
    "Solar photovoltaic",       "Cap|Electricity|Solar|PV (GW)",
    "Concentrated solar power", "Cap|Electricity|Solar|CSP (GW)"
  )

  data <- readSource(type = "IRENA", subtype = "Capacity")[, , mapping$irena]

  # converting MW to GW
  data <- data * 1E-03

  data <- toolAggregate(data, mapping, dim = 3, from = "irena", to = "mif")

  return(list(
    x = data,
    weight = NULL,
    unit = "GW",
    description = "IRENA capacities for technologies geohdr, hydro, wind/on/off, solar/pv/csp"
  ))
}
