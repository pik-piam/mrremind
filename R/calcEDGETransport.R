#' @title Prepare EDGETransport inputs
#'
#' @author Johanna Hoppe
#'
#' @param subtype  REMIND/iterative EDGE-T input data subtypes
#' @return REMIND/iterative EDGE-T input data for all scenario combinations
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "EDGETransport", subtype = "CAPEXandNonFuelOPEX", aggregate = F)
#' }
calcEDGETransport <- function(subtype) {
  x <- readSource("EDGETransport", subtype)

  weightSum <- value <- NULL

  switch(subtype,
    "f35_esCapCost" = {
      weight <- readSource("EDGETransport", subtype = "weightESdemand")
      # Rule out numerical errors after disaggregating very small numbers
      weight[weight < 1e-5] <- 0
      # check whether weightsum is zero for some cases
      # if so, the values should just be aggregated equally in order to prevent zeros in the results
      weight <- rmndt::magpie2dt(weight)
      regMap <- toolGetMapping(getConfig("regionmapping"), type = "regional", where = "mappingfolder")
      data.table::setnames(regMap, "CountryCode", "all_regi")
      weight <- merge(weight, regMap[, c("all_regi", "RegionCode")], by = "all_regi")
      weight[, weightSum := sum(value), by = c("RegionCode", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs", "tall")]
      weight[weightSum == 0, value := 1]
      weight[, c("RegionCode", "weightSum") := NULL]
      weight <- as.magpie(weight)
      unit <- "2017US$/(p|t)km"
      description <- "Capital cost (purchase) per energy service demand on CES level."
      aggregationArguments <- list(zeroWeight = "allow")
    },
    "f35_fe2es" = {
      weight <- readSource("EDGETransport", "f35_demByTech")
      weight[weight < 1e-5] <- 0
      # check whether weightsum is zero for some cases
      # if so, the values should just be aggregated equally in order to prevent zeros in the results
      weight <- rmndt::magpie2dt(weight)
      regMap <- toolGetMapping(getConfig("regionmapping"), type = "regional", where = "mappingfolder")
      data.table::setnames(regMap, "CountryCode", "all_regi")
      weight <- merge(weight, regMap[, c("all_regi", "RegionCode")], by = "all_regi")
      weight[, weightSum := sum(value), by = c("RegionCode", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_enty", "all_in", "all_teEs", "tall")]
      weight[weightSum == 0, value := 1]
      weight[, c("RegionCode", "weightSum", "all_enty", "all_in") := NULL]
      weight <- as.magpie(weight)
      unit <- "trn (p|t)km/Twa"
      description <- "Energy efficiency on CES level."
      aggregationArguments <- list(zeroWeight = "allow")
    },
    "f35_demByTech" = {
      weight <- NULL
      unit <- "TWa"
      description <- "Final energy demand on CES level."
      aggregationArguments <- list()
    },
    "f29_trpdemand" = {
      weight <- NULL
      unit <- "trillion pkm/trillion tkm"
      description <- "Energy service demand on CES level."
      aggregationArguments <- list()
    }
  )

  list(
    x = x,
    weight = weight,
    unit = unit,
    description = description,
    aggregationArguments = aggregationArguments
  )
}
