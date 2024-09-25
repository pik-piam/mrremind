#' calc Project Pipelines
#'
#' Calculate the expected near-term deployment of technologies based on
#' projects that are currently either being built or in a planning stage
#' for some technologies multiple sources are available. Output object currently
#' needs to contain years 2020, 2025 and 2030.
#'
#' Discussions on sources and assumptions:
#' https://github.com/pik-piam/mrremind/discussions
#'
#' @author Pascal Weigmann
#'
#' @param subtype choose technology `biomass`, `coal`, `geothermal`, `hydro`,
#' `nuclear`, `solar`, `wind` or `CCS`
#'
#' @export
calcProjectPipelines <- function(subtype) {
  # CCS ----
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


    # Thresholds
    # formulation of upper and lower bounds for the near-term validation
    # initialize magclass object for thresholds
    t <- new.magpie(getRegions(x),
                    c(2020, 2025, 2030),
                    c("Carbon Management|Storage.min_red",
                      "Carbon Management|Storage.min_yel",
                      "Carbon Management|Storage.max_yel",
                      "Carbon Management|Storage.max_red"),
                    sets = getSets(x))

    # ASSUMPTION[2025, 2030]: min_red
    t[, 2025, "min_red"] <- x[, 2025, "operational"]*0.9
    t[, 2030, "min_red"] <- x[, 2030, "operational"]

    # ASSUMPTION[2025, 2030]: min_yel
    t[, 2025, "min_yel"] <- x[, 2025, "operational"] +
                            x[, 2025, "construction"]*0.2
    t[, 2030, "min_yel"] <- x[, 2030, "operational"] +
                            x[, 2030, "construction"]*0.5

    # ASSUMPTION[2025, 2030]: max_yel
    t[, c(2025, 2030), "max_yel"] <- x[, c(2025, 2030), "operational"] +
                                     x[, c(2025, 2030), "construction"] +
                                     x[, c(2025, 2030), "planned"]*0.2
    # ASSUMPTION[2025, 2030]: max_red
    t[, c(2025, 2030), "max_red"] <- x[, c(2025, 2030), "operational"] +
                                     x[, c(2025, 2030), "construction"] +
                                     x[, c(2025, 2030), "planned"]


    # combine pipeline data with thresholds
    x <- x[, c(2020, 2025, 2030), ]
    x <- mbind(x, t)

    # add meta data
    x <- add_dimension(x, dim = 3.1, add = "model", nm = "IEA CCUS")
    x <- add_dimension(x, dim = 3.4, add = "unit", nm = "MtCO2/yr")
    unit <- "MtCO2/yr"
    description <- "CCS project pipeline from IEA CCUS project database"

    # Hydro ----
  } else if (subtype == "hydro") {
    # Source 1: GEM
    # -> does not include units < 75MW
    # without pumped storage
    x <- readSource("GlobalEnergyMonitor")
    x <- x[, , "Hydro", pmatch = T]

    # TODO: add pumped storage so it can be added to max bounds
    #       -> not comparable to IEA until then

    # initialize magclass object for thresholds
    t <- new.magpie(getRegions(x),
                    c(2020, 2025, 2030),
                    c("GlobalEnergyMonitor.Cap|Electricity|Hydro.min_red.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Hydro.min_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Hydro.max_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Hydro.max_red.GW"),
                    sets = getSets(x))

    # ASSUMPTION: min_red
    t[, , "min_red"] <- x[, , "operating"]

    # ASSUMPTION: min_yel
    t[, , "min_yel"] <- x[, , "operating"] +
                        x[, , "construction"]*0.5 +
                        x[, , "pre-construction"]*0.2

    # ASSUMPTION: max_yel
    t[, , "max_yel"] <- x[, , "operating"] +
                        x[, , "construction"] +
                        x[, , "pre-construction"]*0.8 +
                        x[, , "announced"]*0.3

    # ASSUMPTION: max_red
    t[, , "max_red"] <- x[, , "operating"] +
                        x[, , "construction"] +
                        x[, , "pre-construction"] +
                        x[, , "announced"]

    x <- mbind(x, t)

    # Source 2: IEA Hydropower Special Market Report
    # no access to granular data, only scraping online data explorer,
    # only 2030 available: scenarios "expected" and "accelerated case"
    # pumped storage added to max only (accelerated case has no differentiation,
    # assume pumped storage equally to expected case)
    y <- readSource("IEA_HSMR")

    # initialize magclass object for thresholds
    t <- new.magpie(getRegions(y),
                    c(2020, 2030),
                    c("IEA_HSMR.Cap|Electricity|Hydro.min_red.GW",
                      "IEA_HSMR.Cap|Electricity|Hydro.min_yel.GW",
                      "IEA_HSMR.Cap|Electricity|Hydro.max_yel.GW",
                      "IEA_HSMR.Cap|Electricity|Hydro.max_red.GW"),
                    sets = getSets(y))


    # ASSUMPTION: min_red
    t[, , "min_red"] <- y[, , "operational"]

    # ASSUMPTION: min_yel
    t[, , "min_yel"] <- y[, , "expected"]

    # ASSUMPTION: max_yel
    t[, , "max_yel"] <- y[, , "accelerated"] + y[, , "pumped"]

    # ASSUMPTION: max_red
    t[, , "max_red"] <- y[, , "accelerated"]*1.3 + y[, , "pumped"]

    y <- mbind(y, t)

    # add empty 2025 column, so IEA and GEM data can be merged
    y <- add_columns(y, addnm = c("y2025"), dim = 2, fill= NA)
    x <- mbind(x, y)

    # meta data
    unit <- "GW"
    description <- "Hydro project pipeline from GEM and IEA"

  # TODO: coming up next

  #  Biomass ----
  # } else if (subtype == "biomass") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Biomass", pmatch = T]
  #
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Biomass project pipeline from GEM"

  # Nuclear ----
  } else if (subtype == "nuclear") {
    # Source 1: GEM
    x <- readSource("GlobalEnergyMonitor")
    x <- x[, , "Nuclear", pmatch = T]

    # initialize magclass object for thresholds
    t <- new.magpie(getRegions(x),
                    c(2020, 2025, 2030),
                    c("GlobalEnergyMonitor.Cap|Electricity|Nuclear.min_red.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Nuclear.min_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Nuclear.max_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Nuclear.max_red.GW"),
                    sets = getSets(x))

    # ASSUMPTION: min_yel (only one project in Belarus with start date)
    t[, , "min_yel"] <- x[, , "operating"]*0.8

    # no max_yel, max_red -> would probably make sense to use construction also
    # without start date, otherwise very low upper bounds

    x <- mbind(x, t)

    # Source 2: IEA PRIS
    # doesn't contain dates for expected start of operation
    # -> make assumptions for 2030
    y <- readSource("IEA_PRIS")

    # initialize magclass object for thresholds
    t <- new.magpie(getRegions(y),
                    c(2020, 2025, 2030),
                    c("IEA_PRIS.Cap|Electricity|Nuclear.min_red.GW",
                      "IEA_PRIS.Cap|Electricity|Nuclear.min_yel.GW",
                      "IEA_PRIS.Cap|Electricity|Nuclear.max_yel.GW",
                      "IEA_PRIS.Cap|Electricity|Nuclear.max_red.GW"),
                    sets = getSets(y))

    # ASSUMPTION: min_red
    t[, 2030, "min_red"] <- y[, 2030, "operational"]*0.8

    # ASSUMPTION: min_yel
    t[, 2030, "min_yel"] <- y[, 2030, "operational"]*0.9

    # ASSUMPTION: max_yel
    t[, 2030, "max_yel"] <- y[, 2030, "operational"] + y[, 2030, "construction"]*0.75

    # ASSUMPTION: max_red
    t[, 2030, "max_red"] <- y[, 2030, "operational"] + y[, 2030, "construction"]

    # add empty 2025 and 2030 column, so IEA and GEM data can be merged
    y <- add_columns(y, addnm = c("y2025"), dim = 2, fill= NA)
    y <- mbind(y, t)

    # combine data from both sources
    x <- mbind(x, y)

    # meta data
    unit <- "GW"
    description <- "Nuclear project pipeline from GEM and IEA PRIS"

  #   # Coal ----
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
  #   # Geothermal ----
  # } else if (subtype == "geothermal") {
  #   x <- readSource("GlobalEnergyMonitor")
  #   x <- x[, , "Geothermal", pmatch = T]
  #
  #
  #
  #   # meta data
  #   unit <- "GW"
  #   description <- "Geothermal project pipeline from GEM"
  # Solar ----
  } else if (subtype == "solar") {
    # GEM probably not the best source for solar as smaller units are missing
    x <- readSource("GlobalEnergyMonitor")
    x <- x[, , "Solar", pmatch = T]

    # lower bounds only, as solar can be built quickly
    # TODO: PV/CSP differentiation
    x <- x[, , "Cap|Electricity|Solar"]  # remove PV/CSP for now

    # initialize magclass object for thresholds
    t <- new.magpie(getRegions(x),
                    c(2020, 2025, 2030),
                    c("GlobalEnergyMonitor.Cap|Electricity|Solar.min_red.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Solar.min_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Solar.max_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Solar.max_red.GW"),
                    sets = getSets(x))

    # ASSUMPTION: min_red = operating
    t[, , "min_red"] <- x[, , "operating"]
    # ASSUMPTION: min_yel = operating + 0.5*construction + 0.2*pre-construction
    t[, , "min_yel"] <- x[, , "operating"] +
                        x[, , "construction"]*0.5 +
                        x[, , "pre-construction"]*0.2

    x <- mbind(x, t)

    # meta data
    unit <- "GW"
    description <- "Solar project pipeline from GEM"

    # Wind ----
  } else if (subtype == "wind") {
    # GEM probably not the best source for Wind as smaller units are missing
    x <- readSource("GlobalEnergyMonitor")
    x <- x[, , "Wind", pmatch = T]

    # TODO: On/Offshore differentiation
    x <- x[, , "Cap|Electricity|Wind"]  # remove on/offshore for now


    # initialize magclass object
    t <- new.magpie(getRegions(x),
                    c(2020, 2025, 2030),
                    c("GlobalEnergyMonitor.Cap|Electricity|Wind.min_red.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Wind.min_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Wind.max_yel.GW",
                      "GlobalEnergyMonitor.Cap|Electricity|Wind.max_red.GW"),
                    sets = getSets(x))

    # ASSUMPTION: min_red
    t[, , "min_red"] <- x[, , "operating"]
    # ASSUMPTION: min_yel
    t[, , "min_yel"] <- x[, , "operating"] +
                        x[, , "construction"]*0.5 +
                        x[, , "pre-construction"]*0.2
    # ASSUMPTION: max_yel
    t[, , "max_yel"] <- x[, , "operating"] +
                        x[, , "construction"] +
                        x[, , "pre-construction"]*0.8 +
                        x[, , "announced"]*0.3
    # ASSUMPTION: max_red
    t[, , "max_red"] <- x[, , "operating"] +
                        x[, , "construction"] +
                        x[, , "pre-construction"] +
                        x[, , "announced"]

    x <- mbind(x, t)

    # meta data
    unit <- "GW"
    description <- "Wind project pipeline from GEM"

  }

  return(list(
    x = x,
    unit = unit,
    weight = NULL,
    description = description
    ))
  }
