#' Calculate REMIND variables from historical IRENA capacities.
#'
#' @author Falk Benke
#'
calcIRENA <- function() {
  mapping <- tibble::tribble(
    ~irena,                     ~mif,
    "Geothermal",               "Cap|Electricity|Geothermal (GW)",
    # Note: "Hydropower" contains renewable hydropower and mixed hydro plants, but not pure pumped storage
    "Hydropower",               "Cap|Electricity|Hydro (GW)",
    "Wind",                     "Cap|Electricity|Wind (GW)",
    "Onshore wind energy",      "Cap|Electricity|Wind|Onshore (GW)",
    "Offshore wind energy",     "Cap|Electricity|Wind|Offshore (GW)",
    "Solar",                    "Cap|Electricity|Solar (GW)",
    "Solar photovoltaic",       "Cap|Electricity|Solar|PV (GW)",
    "Concentrated solar power", "Cap|Electricity|Solar|CSP (GW)"
  )

  capacity <- readSource(type = "IRENA", subtype = "Capacity")[, , mapping$irena] %>%
    toolAggregate(rel = mapping, dim = 3, from = "irena", to = "mif") * # renaming to remind names
    1e-3 # converting MW to GW

  return(list(
    x = capacity,
    weight = NULL,
    unit = "GW",
    description = "IRENA capacities for technologies geohdr, hydro, wind/on/off, solar/pv/csp"
  ))
}
