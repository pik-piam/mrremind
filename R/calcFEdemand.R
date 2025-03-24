#' Calculates Final Energy Demand for Industry and Buildings
#' @param scenario GDP and pop scenarios. Passed to [mrdrivers::calcGDP()].
#' @author Falk Benke, Robin Hasse
calcFEdemand <- function(scenario) {

  feBuildings <- calcOutput("FeDemandBuildings",
                            subtype = "FE",
                            scenario = scenario,
                            warnNA = FALSE,
                            aggregate = FALSE)

  feIndustry <- calcOutput("FeDemandIndustry",
                           scenarios = scenario,
                           warnNA = FALSE,
                           aggregate = FALSE)

  t <- intersect(getYears(feBuildings), getYears(feIndustry))

  remind <- mbind(feBuildings[, t, ], feIndustry[, t, ])

  list(
    x = remind,
    weight = NULL,
    unit = glue::glue("EJ, except ue_cement (Gt), ue_primary_steel and ue_secondary_steel (Gt) and ue_chemicals \
                         and ue_otherInd ($tn)"),
    description = "demand pathways for final energy in buildings and industry",
    structure.data = "^(SSP[1-5].*|SDP.*)\\.(fe|ue)"
  )
}
