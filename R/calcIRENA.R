#' Calculate REMIND variables from historical IRENA capacities.
#'
#' @author Falk Benke
#' @export
calcIRENA <- function() {

  data <- readSource(type = "IRENA", subtype = "Capacity")[, , c(
    "Concentrated solar power",
    "Geothermal", "Renewable hydropower",
    "Solar photovoltaic", "Wind"
  )]

  # converting MW to GW
  data <- data * 1E-03

  mapping <- data.frame(
    IRENA_techs =
      c("Concentrated solar power", "Geothermal", "Renewable hydropower", "Solar photovoltaic", "Wind"),
    REMIND_var =
      c(
        "Cap|Electricity|Solar|CSP (GW)", "Cap|Electricity|Geothermal (GW)", "Cap|Electricity|Hydro (GW)",
        "Cap|Electricity|Solar|PV (GW)", "Cap|Electricity|Wind (GW)"
      ), stringsAsFactors = FALSE
  )

  data <- madrat::toolAggregate(data, mapping, dim = 3, from = "IRENA_techs", to = "REMIND_var")

  data <- mbind(
    data,
    setNames(
      data[, , "Cap|Electricity|Solar|CSP (GW)"] + data[, , "Cap|Electricity|Solar|PV (GW)"],
      "Cap|Electricity|Solar (GW)"
    )
  )

  return(list(
    x = data,
    weight = NULL,
    unit = "GW",
    description = "IRENA capacities for technologies csp, geohdr, hydro, spv, wind"
  ))
}
