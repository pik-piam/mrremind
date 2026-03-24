#' FE Share parameters used in REMIND
#'
#' @param subtype 'ind_coal' for the share of coal used in industry.
#' 'ind_bio' for the share of biomass used in industry
#' @param scenario Vector of strings. Used here only to optimize madrat cache usage,
#' as in the end only the 2005 FE demand value is actually used - which is equal across scenarios.
#' @author Antoine Levesque
#'
calcFEShares <- function(subtype, scenario) {

  if (!subtype %in% c("ind_coal", "ind_bio", "ind_liq")) {
    stop("Unknown subtype.")
  }

  edge_buildings <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                               aggregate = FALSE)
  output <- calcOutput("IO", subtype = "output", corrected = TRUE, aggregate = FALSE)

  if (subtype == "ind_coal") {
    share <- 1 - dimSums(edge_buildings[, 2005, "coal"]) / output[, 2005, "pecoal.sesofos.coaltr"]
    weight <- output[, 2005, "pecoal.sesofos.coaltr"]
    descr <- glue::glue("share of coal used in industry (computed by retrieving buildings uses, i.e., considering \\
                        transport and ONONSPEC are null)")
  } else if (subtype == "ind_bio") {
    share <- 1 - dimSums(edge_buildings[, 2005, c("biomod", "biotrad")]) /
      dimSums(output[, 2005, c("pebiolc.sesobio.biotr", "pebiolc.sesobio.biotrmod")], dim = 3)
    weight <- dimSums(output[, 2005, c("pebiolc.sesobio.biotr", "pebiolc.sesobio.biotrmod")], dim = 3)
    descr <- glue::glue("share of biomass used in industry (computed by retrieving buildings uses, i.e., considering \\
                        transport and ONONSPEC are null)")
  } else if (subtype == "ind_liq") {

    # Get FE demand data for 2005 (equal across scenarios, so just pick SSP2).
    ## Keep scenario selection as is (this optimizes madrat cache usage).

    fe_dem_build <- calcOutput("FeDemandBuildings", subtype = "FE", scenario = scenario, aggregate = FALSE)
    fe_dem_ind <- calcOutput("FeDemandIndustry",  scenarios = scenario, aggregate = FALSE)[, , "SSP2"]

    fehoi <- dimSums(fe_dem_ind[, 2005, c("feli_cement", "feli_chemicals", "feli_steel", "feli_otherInd")], dim = 3)

    share <- fehoi / (fehoi + fe_dem_build[, 2005, "SSP2.none.fehob"])
    weight <- (fehoi + fe_dem_build[, 2005, "SSP2.none.fehob"])
    descr <- "share of stationary heating oil used in industry"
  }

  share <- collapseNames(share)
  share[is.na(share)] <- 0
  weight[is.na(weight)] <- 0

  list(x = share,
       weight = weight,
       unit = "fraction",
       description = descr)
}
