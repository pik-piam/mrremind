
# FE_Buildings, UE_Buildings -> FE, UE

#' @author Robin Hasse, Falk Benke

calcFeDemandBuildings <- function(subtype) {

  if (!subtype %in% c("FE", "UE")) {
    stop(paste0("Unsupported subtype:", subtype))
  }

  # Helper Functions ----

  addDimensions <- function(x, dimVals, dimName, dimCode = 3.2) {
    do.call("mbind", lapply(dimVals, function(item) {
      add_dimension(x, dim = dimCode, add = dimName, nm = item)
    }))
  }

  aggTimeSteps <- function(x, nYears = 5) {
    periods <- sort(getYears(x, as.integer = TRUE))
    periodsTarget <- min(periods):max(periods)
    periodsTarget <- periodsTarget[periodsTarget %% nYears == 0]
    periodsMissing <- setdiff(periodsTarget, periods)
    periodsSubN <- sort(union(head(periods, -1)[diff(periods) != nYears],
                              tail(periods, -1)[diff(periods) != nYears]))
    periodsFill <- intersect(periodsTarget,
                             union(periodsSubN, periodsMissing))
    periodsKeep <- setdiff(periodsTarget, periodsFill)

    periodsBuffer <- unique(do.call(c, lapply(periodsFill, function(y) {
      (y - round(nYears / 2 - 0.1)):(y + round(nYears / 2 - 0.1))
    })))
    xBuffer <- time_interpolate(x, periodsBuffer)
    rel <- expand.grid(period = periodsBuffer, periodAgg = periodsFill)
    rel <- rel[abs(rel$period - rel$periodAgg) <= round(nYears / 2 - 0.1), ]
    rel$w <- ifelse(abs(rel$period - rel$periodAgg) < round(nYears / 2 + 0.1), 1, 0.5)
    w <- xBuffer
    for (y in periodsBuffer) w[, y, ] <- unique(rel[rel$period == y, "w"])
    rel$period <- paste0("y", rel$period)
    rel$periodAgg <- paste0("y", rel$periodAgg)
    xAgg <- toolAggregate(xBuffer, rel = rel, weight = w,
                          from = "period", to = "periodAgg", dim = 2)
    mbind(xAgg, x[, periodsKeep, ])
  }

  # Data Processing ----

  stationary <- readSource("EDGE", subtype = "FE_stationary")
  buildings  <- readSource("EDGE", subtype = "FE_buildings")

  # all 2016 values are zero
  buildings <- buildings[, 2016, invert = TRUE]

  # aggregate to 5-year averages to suppress volatility
  buildings <- aggTimeSteps(buildings)
  stationary <- aggTimeSteps(stationary)

  # rename RCP scenarios in buildings
  rcps <- paste0("rcp", gsub("p", "", getItems(buildings, "rcp")))
  rcps <- gsub("rcpfixed", "none", rcps)
  getItems(buildings, "rcp") <- rcps

  # expand stationary to all RCP scenarios
  stationary <- addDimensions(x = stationary, dimVals = rcps, dimName = "rcp", dimCode = 3.2)

  # extrapolate years missing in buildings, but existing in stationary
  misingYearsBuildings <- setdiff(getYears(stationary), getYears(buildings))
  buildings <- time_interpolate(buildings,
                                interpolated_year = misingYearsBuildings,
                                integrate_interpolated_years = TRUE,
                                extrapolation_type = "constant")

  data <- mbind(stationary[, getYears(stationary), ], buildings[, getYears(stationary), ])

  # Prepare Mapping ----

  mapping <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv", where = "mappingfolder")

  # add total buildings electricity demand: feelb = feelcb + feelhpb + feelrhb

  mapping <- rbind(
    mapping,
    mapping %>%
      filter(.data$REMINDitems_out %in% c("feelcb", "feelhpb", "feelrhb")) %>%
      mutate(REMINDitems_out = "feelb")
  )

  mapping <- mapping %>%
    select("EDGEitems", "REMINDitems_out", "weight_Fedemand") %>%
    na.omit() %>%
    filter(EDGEitems %in% getNames(data, dim = "item")) %>%
    distinct()

  if (length(setdiff(getNames(data, dim = "item"), mapping$EDGEitems) > 0)) {
    stop("Not all EDGE items are in the mapping")
  }

  remindVars <- filter(mapping, grepl("^fe..b$|^feel..b$|^feelcb$", .data$REMINDitems_out))
  remindVars <- unique(remindVars$REMINDitems_out)

  # extend mapping for Useful Energy

  if (subtype == "UE") {
    mapping <- mapping %>%
      mutate(EDGEitems = gsub("_fe$", "_ue", .data[["EDGEitems"]]),
             REMINDitems_out = gsub("^fe", "ue", .data[["REMINDitems_out"]])) %>%
      rbind(mapping)
    remindVars <- gsub("^fe", "ue", remindVars)
  }

  # Apply Mapping ----

  remind <- new.magpie(cells_and_regions = getItems(data, dim = 1),
                       years = getYears(data),
                       names = cartesian(getNames(data, dim = "scenario"), rcps, remindVars),
                       sets = getSets(data))

  for (v in remindVars) {

    w <- mapping %>%
      filter(.data$REMINDitems_out == v) %>%
      select(-"REMINDitems_out") %>%
      as.magpie()

    tmp <- mselect(data, item = getNames(w)) * w

    tmp <- dimSums(tmp, dim = "item", na.rm = TRUE) %>%
      add_dimension(dim = 3.3, add = "item", nm = v)

    remind[, , getNames(tmp)] <- tmp
  }

  # Prepare Output ----

  # remove missing NAVIGATE scenarios
  remind <- remind[, , grep("SSP2EU_(NAV|CAMP)_[a-z]*\\.rcp", getItems(remind, 3), value = TRUE), invert = TRUE]

  # change the scenario names for consistency with REMIND sets
  getNames(remind) <- gsub("^SSP", "gdp_SSP", getNames(remind))
  getNames(remind) <- gsub("SDP", "gdp_SDP", getNames(remind))

  # change item names back from UE to FE
  if (subtype == "UE") {
    getItems(remind, "item") <- gsub("^ue", "fe", getItems(remind, "item"))
    description <- "useful energy demand in buildings"
  } else {
    description <- "demand pathways for final energy in buildings and industry in the original file"
  }

  outputStructure <- switch(subtype,
    FE = "^gdp_(SSP[1-5]|SDP).*\\..*\\.fe.*b$",
    UE = "^gdp_(SSP[1-5]|SDP).*\\..*\\.fe.*b$"
  )

  return(list(x = remind,
              weight = NULL,
              unit = "EJ",
              description = description,
              structure.data = outputStructure))

}
