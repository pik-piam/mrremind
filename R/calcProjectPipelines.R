#' calc Project Pipelines
#'
#' Calculate the expected near-term deployment of technologies
#'
#' @author Pascal Weigmann
#'
#' @param subtype choose technology, either `CCS`, `Nuclear`
#'
#' @export
calcProjectPipelines <- function(subtype) {
  if (subtype == "CCS") {
    x <- readSource("IEA_CCUS", subtype = "pipeline")

    # take away 50% of capacities from Norway and UK and shift to EUR
    x[c("NOR", "GBR"), , ] <- x[c("NOR", "GBR"), , ] * 0.5

    mapping <- toolGetMapping("extramapping_EU27.csv",
                              where = "mappingfolder", type = "regional") %>%
      filter(.data$EU27_map == "EU27")
    eu27 <- unique(mapping$CountryCode)

    # sum up EU27 capacities and add half of Norway / UK capacities
    eu27Pool <-
      dimSums(x[eu27, , ], dim = 1) + dimSums(x[c("NOR", "GBR"), , ], dim = 1)
    getItems(eu27Pool, dim = 1) <- "EU27"

    # distribute EU27 pool to the countries according to GDP
    gdp <- calcOutput("GDP", aggregate = FALSE)[eu27, 2020, "gdp_SSP2EU"]
    eu27Pool <- toolAggregate(eu27Pool,
                              rel = mapping, weight = gdp,
                              from = "EU27_map", to = "CountryCode")
    x[eu27, , ] <- eu27Pool

    # formulation of upper and lower bounds for the near-term validation
    x <- mbind(
      x,
    # ASSUMPTION: min_red = operational
      setNames(x[, , "Carbon Management|Storage|operational"],
               "Carbon Management|Storage|min_red"),

    # ASSUMPTION: min_yel = operational + 0.5*construction
      setNames(x[, , "Carbon Management|Storage|operational"] +
               x[, , "Carbon Management|Storage|construction"]*0.5,
               "Carbon Management|Storage|min_yel"),

    # ASSUMPTION: max_yel = operational + construction + 0.3*planned
    setNames(x[, , "Carbon Management|Storage|operational"] +
             x[, , "Carbon Management|Storage|construction"] +
             x[, , "Carbon Management|Storage|planned"]*0.3,
             "Carbon Management|Storage|max_yel"),

    # ASSUMPTION: max_red = operational + construction + planned
    setNames(x[, , "Carbon Management|Storage|operational"] +
             x[, , "Carbon Management|Storage|construction"] +
             x[, , "Carbon Management|Storage|planned"],
             "Carbon Management|Storage|max_red"))

  }

  return(list(
    x = x,
    weight = NULL,
    unit = "MtCO2/yr",
    description = "CCS project pipeline from IEA CCUS project database"
    ))
  }
