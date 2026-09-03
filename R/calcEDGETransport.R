#' @title Prepare EDGETransport inputs
#'
#' @author Johanna Hoppe
#'
#' @param subtype  REMIND/iterative EDGE-T input data subtypes
#' @return REMIND/iterative EDGE-T input data for all scenario combinations
#'
calcEDGETransport <- function(subtype) {

  x <- readSource("EDGETransport", subtype)

  switch(subtype,
    "f35_esCapCost" = {
      weight <- readSource("EDGETransport", subtype = "weightESdemand")
      weight[weight < 1e-5] <- 0
      unit <- "2017US$/(p|t)km"
      description <- "Capital cost (purchase) per energy service demand on CES level."
      aggregationFunction <- function(x, rel, weight) {
        # check whether weightsum is zero for some cases
        # if so, the values should just be aggregated equally in order to prevent zeros in the results
        weight <- weight[rel[["country"]], , ]
        weightSum <- madrat::toolAggregate(weight, rel = rel, weight = NULL)
        expandedSum <- weightSum[rel[["region"]], , ]
        magclass::getItems(expandedSum, dim = 1) <- rel[["country"]]
        weight[!is.na(expandedSum) & expandedSum == 0] <- 1
        madrat::toolAggregate(x, rel = rel, weight = weight)
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
        weight <- weight[rel[["country"]], , ]
        weightSum <- madrat::toolAggregate(weight, rel = rel, weight = NULL)
        expandedSum <- weightSum[rel[["region"]], , ]
        magclass::getItems(expandedSum, dim = 1) <- rel[["country"]]
        weight[!is.na(expandedSum) & expandedSum == 0] <- 1
        weight <- magclass::dimSums(weight, dim = c("all_enty", "all_in"))
        madrat::toolAggregate(x, rel = rel, weight = weight)
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
