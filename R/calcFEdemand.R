#' Calculates Final Energy Demand for Industry, Buildings and Transport
#' @author Falk Benke
calcFEdemand <- function() {

  feBuildings <- calcOutput("FeDemandBuildings", subtype = "FE", warnNA = FALSE, aggregate = FALSE)
  feIndustry <- calcOutput("FeDemandIndustry", warnNA = FALSE, aggregate = FALSE)
  feTransport <- calcOutput("FeDemandTransport", warnNA = FALSE, aggregate = FALSE)

  # add up industry and buildings contributions to stationary
  stationaryItems <- c("fehes", "feh2s")
  feStationary <- feIndustry[, , stationaryItems] + feBuildings[, , stationaryItems]

  remind <- mbind(
    feBuildings[, , stationaryItems, invert = TRUE],
    feIndustry[, , stationaryItems, invert = TRUE],
    feStationary,
    feTransport
  )

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
