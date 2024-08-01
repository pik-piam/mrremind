#' calc CCS capacity
#'
#' Calculate CCS capacity from IEA CCUS data
#'
#' @author Anne Merfort, Falk Benke
#'
#' @param subtype either `historical` for data until 2022 or `projections`
#' for projections in 2020, 2025 and 2030 (including some redistribution on EU/NEU level)
#'
#' @export
calcCCScapacity <- function(subtype) {
  x <- readSource("IEA_CCUS", subtype = subtype)

  # shift around capacities for EUR for REMIND input data
  if (subtype == "projections") {
    # take away 50% of capacities from Norway and UK
    x[c("NOR", "GBR"), , ] <- x[c("NOR", "GBR"), , ] * 0.5

    mapping <- toolGetMapping("extramapping_EU27.csv", where = "mappingfolder", type = "regional") %>%
      filter(.data$EU27_map == "EU27")
    eu27 <- unique(mapping$CountryCode)

    # sum up EU27 capacities and add half of Norway / UK capacities
    eu27Pool <- dimSums(x[eu27, , ], dim = 1) + dimSums(x[c("NOR", "GBR"), , ], dim = 1)
    getItems(eu27Pool, dim = 1) <- "EU27"

    # distribute EU27 pool to the countries according to GDP
    gdp <- calcOutput("GDP", aggregate = FALSE)[eu27, 2020, "gdp_SSP2"]
    eu27Pool <- toolAggregate(eu27Pool,
      rel = mapping, weight = gdp,
      from = "EU27_map", to = "CountryCode"
    )
    x[eu27, , ] <- eu27Pool
  }

  if (subtype == "historical") {
    x <- x[, seq(2005, max(getYears(x, as.integer = TRUE))), ]
  }

  return(list(
    x = x,
    weight = NULL,
    unit = "MtCO2/yr",
    description = "CCS capacity derived from IEA CCUS project database"
  ))
}
