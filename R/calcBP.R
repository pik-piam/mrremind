#' Calculate REMIND variables from historical BP values
#'
#' @author Falk Benke
#'
calcBP <- function() {
  # read in emissions, capacity, generation, price ----

  emissions <- readSource("BP", subtype = "Emission")

  capacity <- readSource("BP", subtype = "Capacity")
  capacity <- matchDim(capacity, emissions, dim = 2, fill = NA)

  generation <- readSource("BP", subtype = "Generation")

  price <- readSource("BP", subtype = "Price")
  price <- matchDim(price, emissions, dim = 2, fill = NA)

  # prepare consumption data ----
  consumption <- readSource("BP", subtype = "Consumption")

  # accounting of the source as of the 2025 Statistical Review (see the notes below the
  # sheet "Total Energy Supply (TES) -EJ"): non-combustible renewables (solar, wind, hydro)
  # are reported on the energy content of their gross electrical output, i.e. already in
  # direct equivalent accounting, while nuclear is reported on its input heat requirement
  # using a fixed efficiency of 33% and thus needs to be converted

  nuclearEfficiency <- 0.33

  # recalculate nuclear to direct equivalent accounting
  consumption.nuclear <- consumption[, , "Nuclear Consumption (EJ)"] * nuclearEfficiency

  # store the other variables, which need no conversion
  consumption.other <- consumption[, , c("Nuclear Consumption (EJ)",
                                         "Primary Energy Consumption (EJ)"), invert = TRUE]

  # recalculate total pe consumption to direct equivalent accounting

  # 1. deduct original nuclear
  pe.nuclear <- consumption[, , "Nuclear Consumption (EJ)"]
  consumption.pe <- consumption[, , "Primary Energy Consumption (EJ)"] - pe.nuclear

  # 2. add adjusted nuclear value
  pe.nuclear.dea <- pe.nuclear * nuclearEfficiency
  consumption.pe <- consumption.pe + pe.nuclear.dea
  consumption.pe <- collapseDim(consumption.pe)
  getNames(consumption.pe) <- "Primary Energy Consumption (EJ)"
  getSets(consumption.pe) <- c("region", "year", "data")

  # prepare trade data ----

  # calculate net oil trade
  trade.oil <- readSource("BP", subtype = "Trade Oil")
  trade.oil <- matchDim(trade.oil, emissions, dim = 2, fill = NA)

  trade.oil.net <- trade.oil[, , "Trade|Export|Oil (kb/d)"] - trade.oil[, , "Trade|Import|Oil (kb/d)"]
  getNames(trade.oil.net) <- c("Net Trade|Oil (kb/d)")
  getSets(trade.oil.net) <- c("region", "year", "data")

  # calculate net coal trade
  trade.coal <- readSource("BP", subtype = "Trade Coal")
  trade.coal <- matchDim(trade.coal, emissions, dim = 2, fill = NA)

  trade.coal.net <- trade.coal[, , "Trade|Export|Coal (EJ)"] - trade.coal[, , "Trade|Import|Coal (EJ)"]
  getNames(trade.coal.net) <- c("Net Trade|Coal (EJ)")
  getSets(trade.coal.net) <- c("region", "year", "data")

  # calculate net gas trade
  trade.gas <- readSource("BP", subtype = "Trade Gas")
  trade.gas <- matchDim(trade.gas, emissions, dim = 2, fill = NA)

  trade.gas.net <- trade.gas[, , "Trade|Export|Gas (bcm)"] - trade.gas[, , "Trade|Import|Gas (bcm)"]
  getNames(trade.gas.net) <- c("Net Trade|Gas (bcm)")
  getSets(trade.gas.net) <- c("region", "year", "data")

  # map to REMIND variables ----

  x <- mbind(
    emissions,
    capacity,
    generation,
    consumption.other,
    consumption.nuclear,
    consumption.pe,
    trade.oil,
    trade.oil.net,
    trade.gas,
    trade.gas.net,
    trade.coal,
    trade.coal.net,
    price
  )

  map <- toolGetMapping("Mapping_BP.csv", type = "reportingVariables", where = "mrremind") %>%
    filter(!is.na(.data$REMIND), .data$REMIND != "") %>%
    mutate(
      "from" = paste0(trimws(.data$variable), " (", .data$unit, ")"),
      "to" = paste0(trimws(.data$REMIND), " (", .data$Unit_REMIND, ")"),
      "conversion" = as.numeric(.data$Factor)
    ) %>%
    select("from", "to", "conversion")


  for (var in intersect(getNames(x, dim = 1), unique(map$from))) {
    conv <- map[map$from == var, "conversion"]

    # there should be a distinct conversion factor in the mapping
    # if there is more than one conversion factor, it means that one source variable
    # is converted two more than one target variable using a different conversion
    # this case is not covered by the logic
    if (length(unique(conv)) > 1) {
      stop(paste0("Cannot apply conversion factor for variable ", var))
    }

    x[, , var] <- x[, , var] * unique(conv)
  }

  x <- toolAggregate(x,
    dim = 3.1, rel = map, from = "from", partrel = TRUE,
    to = "to", verbosity = 2
  )

  # convert price units ----

  # should be from US$2025, but 'toolConvertGDP' currently returns NA
  poil <- GDPuc::toolConvertGDP(
    gdp = x[, , "Price|Primary Energy|Oil (US$2025/GJ)"],
    unit_in = "constant 2022 US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs = "with_USA"
  )

  getNames(poil) <- gsub("\\$2025", "\\$2017", getNames(poil))

  # assume these units are in current MER
  pcoalgas <- GDPuc::toolConvertGDP(
    gdp = x[, , c("Price|Primary Energy|Gas (US$/GJ)", "Price|Primary Energy|Coal (US$/GJ)")],
    unit_in = "current US$MER",
    unit_out = mrdrivers::toolGetUnitDollar(),
    replace_NAs =  "with_USA"
  )

  getNames(pcoalgas) <- gsub("\\$", "\\$2017", getNames(pcoalgas))


  x <- x[, , c("Price|Primary Energy|Oil (US$2025/GJ)",
               "Price|Primary Energy|Gas (US$/GJ)",
               "Price|Primary Energy|Coal (US$/GJ)"), invert = TRUE]

  x <- mbind(x, poil, pcoalgas)

  # set weights ----

  weights <- x
  weights[, , ] <- NA
  weights[, , "US$2017/GJ", pmatch = TRUE] <- 1

  return(list(
    x = x,
    weight = weights,
    mixed_aggregation = TRUE,
    unit = c("Mt CO2/yr", "GW", "EJ/yr", "US$2017/GJ"),
    description = "Historical World Energy Statistics values as REMIND variables"
  ))
}
