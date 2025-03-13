#' FE Share parameters used in REMIND
#'
#' @param subtype 'ind_coal' for the share of coal used in industry. 'ind_bio' for the share of biomass used in industry
#' @param scenario Vector of strings. Used here only to optimize madrat cache usage, as in the end only the 2005 FEdemand
#' value is actually used - which is equal across scenarios.
#' @author Antoine Levesque
#'
calcFEShares <- function(subtype, scenario) {
  if (! subtype %in% c("ind_coal", "ind_bio", "ind_liq")) {
    stop("Unknown subtype.")
  }

  edge_buildings <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)
  output <- calcOutput("IO", subtype = "output", aggregate = FALSE)
  # Get FEdemand data for 2005 (equal across scenarios, so just pick SSP2).
  ## Keep scenario selection as is (this optimizes madrat cache usage).
  fe_demand <- calcOutput("FEdemand", scenario = scenario, aggregate = FALSE)[, 2005, "SSP2"] %>% collapseNames()

  if (subtype == "ind_coal") {
    share <- 1 - dimSums(edge_buildings[, 2005, "coal"]) / output[, 2005, "pecoal.sesofos.coaltr"]
    weight <- output[, 2005, "pecoal.sesofos.coaltr"]
    descr <- glue::glue("share of coal used in industry (computed by retrieving buildings uses, i.e., considering \\
                        transport and ONONSPEC are null)")
  }

  if (subtype == "ind_bio") {
    share <- 1 - dimSums(edge_buildings[, 2005, c("biomod", "biotrad")]) /
      dimSums(output[, 2005, c("pebiolc.sesobio.biotr", "pebiolc.sesobio.biotrmod")], dim = 3)
    weight <- dimSums(output[, 2005, c("pebiolc.sesobio.biotr", "pebiolc.sesobio.biotrmod")], dim = 3)
    descr <- glue::glue("share of biomass used in industry (computed by retrieving buildings uses, i.e., considering \\
                        transport and ONONSPEC are null)")
  }

  if (subtype == "ind_liq") {
    fehoi <- dimSums(fe_demand[, 2005, c("feli_cement", "feli_chemicals", "feli_steel", "feli_otherInd")])
    share <- fehoi / (fehoi + fe_demand[, 2005, c("fehob")])
    weight <- (fehoi + fe_demand[, 2005, c("fehob")])
    descr <- "share of stationary heating oil used in industry"
  }

  share <- collapseNames(share)
  share[is.na(share)] <- 0
  weight[is.na(weight)] <- 0

  list(x = share,
       weight = weight,
       unit = "dimensionless",
       description = descr)
}
