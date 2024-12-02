#' Calculates Final Energy Demand for Industry and Buildings
#' @author Falk Benke
calcFEdemand <- function() {

  feBuildings <- calcOutput("FeDemandBuildings", subtype = "FE", warnNA = FALSE, aggregate = FALSE)

  # extrapolate missing years
  feBuildings <- time_interpolate(feBuildings, interpolated_year = seq(1995, 2150, 5),
                                  extrapolation_type = "constant")

  feIndustry <- calcOutput("FeDemandIndustry", warnNA = FALSE, aggregate = FALSE)

  # duplicate scenarios ----

  # add Navigate and Campaigners scenarios to industry to match buildings scenarios by duplication
  duplicateScens <- "gdp_SSP2EU_NAV_all"
  feIndustry <- mbind(feIndustry, setItems(feIndustry[, , "gdp_SSP2EU"], 3.1, duplicateScens))

  remind <- mbind(feBuildings, feIndustry)

  return(list(
    x = remind,
    weight = NULL,
    unit = paste0(
      "EJ, except ue_cement (Gt), ue_primary_steel and ",
      "ue_secondary_steel (Gt) and ue_chemicals and ",
      "ue_otherInd ($tn)"
    ),
    description = "demand pathways for final energy in buildings and industry",
    structure.data = "^gdp_(SSP[1-5].*|SDP.*)\\.(fe|ue)"
  ))
}
