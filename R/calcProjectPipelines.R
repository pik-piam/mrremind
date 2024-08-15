#' calc Project Pipelines
#'
#' Calculate the expected near-term deployment of technologies based on
#' projects that are currently either being built or in a planning stage
#' for some technologies multiple sources are available
#'
#' @author Pascal Weigmann
#'
#' @param subtype choose technology `biomass`, `coal`, `geothermal`, `hydro`,
#' `nuclear`, `solar`, `wind` or `CCS`
#'
#' @export
calcProjectPipelines <- function(subtype) {
  # CCS ####
  # Discussion about CCS assumptions
  # https://gitlab.pik-potsdam.de/REMIND/committed/-/issues/1
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

    # ASSUMPTION: no projects are under way in Brasil which means all thresholds are equal
    # to avoid this, add manually an upper estimate of what could still be planned in BRA
    x["BRA", 2025:2029, "Carbon Management|Storage.planned"] <-
      x["BRA", 2025:2029, "Carbon Management|Storage.planned"] + 2
    x["BRA", 2030, "Carbon Management|Storage.planned"] <-
      x["BRA", 2030, "Carbon Management|Storage.planned"] + 10


    # formulation of upper and lower bounds for the near-term validation
    x <- mbind(
      x,
    # ASSUMPTION: min_red = operational
      setNames(x[, , "Carbon Management|Storage.operational"],
               "Carbon Management|Storage.min_red"),

    # ASSUMPTION: min_yel = operational + 0.5*construction
      setNames(x[, , "Carbon Management|Storage.operational"] +
               x[, , "Carbon Management|Storage.construction"]*0.5,
               "Carbon Management|Storage.min_yel"),

    # ASSUMPTION: max_yel = operational + construction + 0.3*planned
    setNames(x[, , "Carbon Management|Storage.operational"] +
             x[, , "Carbon Management|Storage.construction"] +
             x[, , "Carbon Management|Storage.planned"]*0.3,
             "Carbon Management|Storage.max_yel"),

    # ASSUMPTION: max_red = operational + construction + planned
    setNames(x[, , "Carbon Management|Storage.operational"] +
             x[, , "Carbon Management|Storage.construction"] +
             x[, , "Carbon Management|Storage.planned"],
             "Carbon Management|Storage.max_red"))

    # meta data
    x <- add_dimension(x, dim = 3.1, add = "model", nm = "IEA CCUS")
    x <- add_dimension(x, dim = 3.4, add = "unit", nm = "MtCO2/yr")
    unit <- "MtCO2/yr"
    description <- "CCS project pipeline from IEA CCUS project database"

    # Hydro ####
    # Discussion about Hydro assumptions
    # https://gitlab.pik-potsdam.de/REMIND/committed/-/issues/2
  } else if (subtype == "hydro") {
    # without pumped storage
    x <- readSource("GlobalEnergyMonitor")
    x <- x[, , "Hydro", pmatch = T]

    x <- mbind(x,
      # ASSUMPTION: min_red = operating
      setNames(x[, , "operating"],
               "GlobalEnergyMonitor.Cap|Electricity|Hydro.min_red.GW"),

      # ASSUMPTION: min_yel = operating + 0.5*construction + 0.2*pre-construction
      setNames(x[, , "operating"] +
                 x[, , "construction"]*0.5 +
                 x[, , "pre-construction"]*0.2,
               "GlobalEnergyMonitor.Cap|Electricity|Hydro.min_yel.GW"),

      # ASSUMPTION: max_yel = operating + construction + 0.8*pre-construction + 0.3*announced
      setNames(x[, , "operating"] +
                 x[, , "construction"] +
                 x[, , "pre-construction"]*0.8 +
                 x[, , "announced"]*0.3,
               "GlobalEnergyMonitor.Cap|Electricity|Hydro.max_yel.GW"),

      # ASSUMPTION: max_red = operating + construction + pre-construction + announced
      setNames(x[, , "operating"] +
                 x[, , "construction"] +
                 x[, , "pre-construction"] +
                 x[, , "announced"],
               "GlobalEnergyMonitor.Cap|Electricity|Hydro.max_red.GW")
    )

    # meta data
    unit <- "GW"
    description <- "Hydro project pipeline from GEM"

  # TODO: coming up next
  #   # Biomass ####
  # } else if (subtype == "biomass") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Biomass", pmatch = T]
  #
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Biomass project pipeline from GEM"
  #
  #   # Nuclear ####
  # } else if (subtype == "nuclear") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Nuclear", pmatch = T]
  #
  #
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Nuclear project pipeline from GEM"
  #
  #   # Coal ####
  # } else if (subtype == "coal") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Coal", pmatch = T]
  #
  #
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Coal project pipeline from GEM"
  #
  #   # Geothermal ####
  # } else if (subtype == "geothermal") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Geothermal", pmatch = T]
  #
  #
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Geothermal project pipeline from GEM"
  #   # Solar ####
  # } else if (subtype == "solar") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Solar", pmatch = T]
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Solar project pipeline from GEM"
  #
  #   # Wind ####
  # } else if (subtype == "wind") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Wind", pmatch = T]
  #
  #   #
  #
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Wind project pipeline from GEM"
  #
  }

  x <- x[ ,c(2025, 2030), ]

  return(list(
    x = x,
    unit = unit,
    weight = NULL,
    description = description
    ))
  }
