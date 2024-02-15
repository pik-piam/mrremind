#' Calculates FE demand in transport as REMIND variables
#'
#' @author Alois Dirnaicher, Johanna Hoppe
calcFeDemandTransport <- function() {

  # Read in stationary data and map to REMIND variables ----

  # REMIND transport items
  trp_nodes <- c("ueelTt", "ueLDVt", "ueHDVt")

  data <- readSource("Stationary")

  # aggregate to 5-year averages to suppress volatility
  data <- toolAggregateTimeSteps(data)

  mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                            where = "mrcommons")

  mapping <- mapping %>%
    select("EDGEitems", "REMINDitems_out", "weight_Fedemand") %>%
    na.omit() %>%
    filter(.data$EDGEitems %in% getNames(data, dim = "item"),
           .data$REMINDitems_out %in% trp_nodes) %>%
    distinct()

  if (length(setdiff(trp_nodes, mapping$REMINDitems_out) > 0)) {
    stop("Not all transport items are in the mapping")
  }

  remind <- new.magpie(cells_and_regions = getItems(data, dim = 1),
                       years = getYears(data),
                       names = cartesian(getNames(data, dim = "scenario"),
                                         unique(mapping$REMINDitems_out)),
                       sets = getSets(data))

  for (v in unique(mapping$REMINDitems_out)) {

    w <- mapping %>%
      filter(.data$REMINDitems_out == v) %>%
      select(-"REMINDitems_out") %>%
      as.magpie()

    tmp <- mselect(data, item = getNames(w)) * w

    tmp <- dimSums(tmp, dim = "item", na.rm = TRUE) %>%
      add_dimension(dim = 3.3, add = "item", nm = v)

    remind[, , getNames(tmp)] <- tmp
  }

  # change the scenario names for consistency with REMIND sets
  getNames(remind) <- gsub("^SSP", "gdp_SSP", getNames(remind))
  getNames(remind) <- gsub("SDP", "gdp_SDP", getNames(remind))

  # Corrections for gdp_SDP  ----

  ## Start of actual function

  ## adding dummy vars and functions to avoid global var complaints
  year <- scenario <- item <- value <- .SD <- dem_cap <- fact <-
    toadd <- Year <- gdp_cap <- ssp2dem <- window <- train_add <- NULL

  ## we work in the REMIND H12 regions to avoid strange ISO country behavior when rescaling
  mappingfile <- toolGetMapping(
    type = "regional", name = "regionmappingH12.csv",
    returnPathOnly = TRUE, where = "mappingfolder"
  )

  rmnd_reg <- toolAggregate(remind, mappingfile, from = "CountryCode", to = "RegionCode")

  ## convert to data.table (we use gdp_SSP2 as a starting point)
  trpdem <- rmnd_reg %>%
    as.quitte() %>%
    select("scenario", "region", "year" = "period", "item", "value") %>%
    filter(.data$scenario == "gdp_SSP2") %>%
    mutate("scenario" = "gdp_SDP") %>%
    as.data.table()

  ## get population
  pop <- data.table::as.data.table(calcOutput("Population"))[
    , year := as.numeric(gsub("y", "", year))]
  data.table::setnames(pop, c("variable", "iso3c"), c("scenario", "region"),
                       skip_absent = TRUE)

  ## intrapolate missing years
  yrs <- sort(union(pop$year, trpdem$year))
  pop <- pop[data.table::CJ(region = pop$region, year = yrs, scenario = pop$scenario, unique = TRUE),
             on = c("region", "year", "scenario")]
  pop[, value := stats::approx(x = .SD$year, y = .SD$value, xout = .SD$year)$y,
      by = c("region", "scenario")]

  ## merge scenario names
  pop[, scenario := gsub("pop_", "gdp_", scenario)]
  data.table::setnames(pop, "value", "pop")

  demPop <- pop[trpdem, on = c("year", "region", "scenario")]
  demPop[, dem_cap := value / pop * 1e3] # EJ/10^6=TJ (pop. in millions), scale to GJ/cap*yr

  gdp_iso <- calcOutput("GDP", aggregate = FALSE)[, , "gdp_SDP"]
  gdp_iso <- time_interpolate(gdp_iso, getYears(rmnd_reg))
  gdp_reg <- toolAggregate(gdp_iso, mappingfile, from = "CountryCode", to = "RegionCode")
  getSets(gdp_reg) <- c("region", "Year", "scenario")

  ## load GDP
  gdp <- data.table::as.data.table(gdp_reg)[
    , year := as.numeric(gsub("y", "", Year))][
      , Year := NULL]

  data.table::setnames(gdp, "value", "gdp")

  ## merge
  demPop <- gdp[demPop, on = c("year", "region", "scenario")]
  demPop[, gdp_cap := gdp / pop]

  ## add new scenario from SSP2
  newdem <- demPop

  data.table::setkey(newdem, "year", "item")
  newdem[, ssp2dem := dem_cap]
  for (yr in seq(2025, 2100, 5)) {
    it <- "ueLDVt"
    target <- 7 ## GJ
    switch_yrs <- 10
    drive <- 0.12
    prv_row <- newdem[year == yr - 5 & item == it]
    newdem[year == yr & item == it,
           window := ifelse(prv_row$dem_cap - target >= 0,
                            drive * pmin((prv_row$dem_cap - target)^2 / target^2, 0.2),
                            -drive * (target - prv_row$dem_cap) / target)]

    newdem[year == yr & item == it,
           dem_cap := (1 - window)^5 * prv_row$dem_cap * pmin((yr - 2020) / switch_yrs, 1) + ssp2dem * (1 - pmin((yr - 2020) / switch_yrs, 1))]

    it <- "ueHDVt"
    target <- 9
    prv_row <- newdem[year == yr - 5 & item == it]
    newdem[year == yr & item == it,
           window := ifelse(prv_row$dem_cap - target >= 0,
                            drive * pmin((prv_row$dem_cap - target)^2 / target^2, 0.2),
                            -drive * (target - prv_row$dem_cap) / target)]
    newdem[year == yr & item == it,
           dem_cap := (1 - window)^5 * prv_row$dem_cap * pmin((yr - 2020) / switch_yrs, 1) + ssp2dem * (1 - pmin((yr - 2020) / switch_yrs, 1))]

  }

  newdem[, c("window", "ssp2dem") := NULL]

  ## add trains
  trns <- function(year) {
    if (year <= 2020)
      return(0)
    else
      return((year - 2020)^2 * 0.000018) # at 2100, this is ~ 11.5%
  }

  yrs <- unique(newdem$year)
  trainsdt <- data.table::data.table(year = yrs, fact = sapply(yrs, trns))

  newdem <- newdem[trainsdt, on = "year"]

  ## both freight and passenger road are reduced in favour of trains
  newdem[item %in% c("ueHDVt", "ueLDVt"), toadd := dem_cap * fact]
  newdem[item %in% c("ueHDVt", "ueLDVt"), dem_cap := dem_cap - toadd]

  newdem[, train_add := 0]
  newdem[item == "ueelTt" & year > 2025, dem_cap := newdem[item == "ueelTt" & year == 2025]$dem_cap, by = year]

  ## we add it to trains
  newdem[year > 2020, train_add := sum(toadd, na.rm = TRUE),
         by = c("year", "region")]
  ## replace old values
  newdem[item == "ueelTt", dem_cap := dem_cap + train_add][, c("toadd", "train_add", "fact") := NULL]

  ## multiply by population
  newdem[, value := dem_cap * pop / 1e3] # back to EJ

  ## constant for t>2100
  newdem[year > 2100, value := newdem[year == 2100]$value, by = "year"]

  newdem <- suppressWarnings(as.magpie(newdem[, c("region", "year", "scenario", "item", "value")]))
  dem_iso <- toolAggregate(newdem, mappingfile, gdp_iso, from = "RegionCode", to = "CountryCode")
  getSets(dem_iso)[1] <- "region"

  # Prepare Output ----

  # replace SDP data calculated in readSource("Stationary") with corrected data
  remind <- mbind(remind[, , getNames(dem_iso), invert = TRUE], dem_iso)

  return(list(x = remind, weight = NULL, unit = "EJ",
              description = "final energy demand in transport"))

}
