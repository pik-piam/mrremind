#' @title Prepare EDGETransport inputs
#'
#' @author Johanna Hoppe
#'
#' @param subtype  REMIND/iterative EDGE-T input data subtypes
#' @return REMIND/iterative EDGE-T input data for all scenario combinations
#'
calcEDGETransport <- function(subtype) {

  x <- readSource("EDGETransport", subtype)

  weightSum <- value <- NULL

  switch(subtype,
    "f35_esCapCost" = {
      weight <- readSource("EDGETransport", subtype = "weightESdemand")
      weight[weight < 1e-5] <- 0
      unit <- "2017US$/(p|t)km"
      description <- "Capital cost (purchase) per energy service demand on CES level."

      aggregationFunction <- function(x, rel, weight) {

        # check whether weightsum is zero for some cases
        # if so, the values should just be aggregated equally in order to prevent zeros in the results
        weight <- rmndt::magpie2dt(weight)
        regMap <- rel
        data.table::setnames(regMap, "country", "all_regi")
        weight <- merge(weight, regMap[, c("all_regi", "region")], by = "all_regi")
        weight[, weightSum := sum(value), by = c("region", "GDP_scenario", "DEM_scenario",
                                                 "EDGE_scenario", "all_teEs", "tall")]
        weight[weightSum == 0, value := 1]
        weight[, c("region", "weightSum") := NULL]
        weight <- as.magpie(weight)
        toolAggregate(x, rel = rel, weight)
      }

    },
    "f35_fe2es" = {
      weight <- readSource("EDGETransport", "f35_demByTech")
      weight[weight < 1e-5] <- 0
      unit <- "trn (p|t)km/Twa"
      description <- "Energy efficiency on CES level."

      aggregationFunction <- function(x, rel, weight) {

        # check whether weightsum is zero for some cases
        # if so, the values should just be aggregated equally in order to prevent zeros in the results
        weight <- rmndt::magpie2dt(weight)
        regMap <- rel
        data.table::setnames(regMap, "country", "all_regi")
        weight <- merge(weight, regMap[, c("all_regi", "region")], by = "all_regi")
        weight[, weightSum := sum(value), by = c("region", "GDP_scenario", "DEM_scenario",
                                                 "EDGE_scenario", "all_enty", "all_in", "all_teEs", "tall")]
        weight[weightSum == 0, value := 1]
        weight[, c("region", "weightSum", "all_enty", "all_in") := NULL]
        weight <- as.magpie(weight)
        toolAggregate(x, rel = rel, weight)
      }

    },
    "f35_demByTech" = {
      weight <- NULL
      unit <- "TWa"
      description <- "Final energy demand on CES level."
      aggregationFunction = madrat::toolAggregate
    },
    "f29_trpdemand" = {
      weight <- NULL
      unit <- "trillion pkm/trillion tkm"
      description <- "Energy service demand on CES level."
      aggregationFunction = madrat::toolAggregate
    }
  )

  list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    aggregationFunction = aggregationFunction
  )
}
