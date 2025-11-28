#' Calculate NDC Emissions Targets
#'
#' @description This function calculates the emissions targets for the NDC scenarios applied in the REMIND module 45_carbonprice realization NDC.
#' It contains the following steps:
#' 1. Read country-level NDC targets as target factors (target year emissions normalized by 2015 emissions).
#' 2. Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets (yet).
#' 3. Aggregate country-level target factors to region-level target factors using 2015 emissions as weight ("Ghgfactor").
#' 4. Calculate share of emissions covered under NDC target per REMIND region ("Ghgshare").
#' The parameters calculated in 3.) and 4.) are further used in the NDC realization to calculate the region-wide NDC emissions targets
#' in terms of total GHG emissions excl. bunkers and excl. LULUCF sectors.
#'
#' @param sources database source, either 'UNFCCC_NDC' or 'NewClimate'
#' @param subtype must be one of
#' - 'Ghgfactor': target factors (target year emissions normalized by 2015 emissions)
#' - 'Ghgshare': share of emissions covered under NDC target per REMIND region
#' @param scenario GDP and pop scenarios. Passed to [mrdrivers::calcGDP()].
#' turned off for inpudata generation
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Rahel Mandaroux, Falk Benke
#'
calcEmiTarget <- function(sources, subtype, scenario) {
# Main steps:
# 1. Read country-level NDC targets as target factors (target year emissions normalized by 2015 emissions)
# 2. Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets (yet)
# 3. Aggregate country-level target factors to region-level target factors using 2015 emissions as weight ("Ghgfactor")
# 4. Calculate share of emissions covered under NDC target per REMIND region ("Ghgshare")


# 1. Read country-level NDC targets as target factors ----

  # check whether valid sources and subtypes chosen
  if (!sources %in% c("UNFCCC_NDC", "NewClimate")) {
    stop("Unknown source ", sources, " for calcEmiTarget.")
  }

  if (!subtype %in% c("Ghgshare", "Ghgfactor")) {
    stop("Unknown 'subtype' argument")
  }

  # Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs", to calls of the individual scenarios.
  scenario <- mrdrivers::toolReplaceShortcuts(scenario) %>% unique()

  # Reference Emissions from CEDS used for aggregation weights
  ghg <- calcOutput("EmiTargetReference", aggregate = FALSE)[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"]

  # read country-level target factors from sources and save as list
  if (sources == "UNFCCC_NDC") {
    listGhgFactors <- list(
      "2018_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2018_cond", subset = scenario),
      "2018_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2018_uncond", subset = scenario),
      "2021_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2021_cond", subset = scenario),
      "2021_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2021_uncond", subset = scenario),
      "2022_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2022_cond", subset = scenario),
      "2022_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2022_uncond", subset = scenario),
      "2023_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2023_cond", subset = scenario),
      "2023_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2023_uncond", subset = scenario),
      "2024_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2024_cond", subset = scenario),
      "2024_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2024_uncond", subset = scenario),
      "2025_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2025_cond", subset = scenario),
      "2025_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2025_uncond", subset = scenario)
    )
  }

  if (sources == "NewClimate") {
    listGhgFactors <- list(
      "2025_cond"   = readSource("NewClimate", subtype = "Emissions_2025_cond", subset = scenario),
      "2025_uncond" = readSource("NewClimate", subtype = "Emissions_2025_uncond", subset = scenario)
    )
  }

  # convert target factors to magclass objects
  # ensure that all magclass objects in the list have matching years so they can be bound together
  listYears <- lapply(listGhgFactors, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
  ghgFactor <- purrr::map(listGhgFactors,
                          ~ add_columns(.x, listYears[!listYears %in% getItems(.x, dim = "year")], 2))
  ghgFactor <- mbind(ghgFactor)
  ghgFactor <- ghgFactor[, sort(getYears(ghgFactor)), ]


  # 2. Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets ----

  # add NDC versions "Emissions_NDC " and "Emissions_2025_uncond_extrapol"
  # that take the 2035 targets from the 2025 version, but extrapolate 2030 targets to 2035 for countries which do not have 2035 targets yet
  # create new NDC version 2025 with extrapolated targets

  # if no 2035 targets available, add 2035 to target factor object
  if (! 2035 %in% getYears(ghgFactor, as.integer = TRUE)) {
    ghgFactor <- add_columns(ghgFactor, addnm = "y2035", dim = 2, fill = NA)
  }
  # create maglcass object for extrapolated NDC scenarios
  ghgFactorExtrapolated <- ghgFactor[, , c("2025_cond", "2025_uncond")]
  getItems(ghgFactorExtrapolated, dim=3.1) <- paste0(getItems(ghgFactorExtrapolated, dim=3.1),"_extrapol")
  # Explanation of the extrapolation:
  # Note that the ghgFactor gives the remaining relative emissions relative to 2015.
  # Hence, 1 - ghgFactor gives the relative emissions reductions relative to 2015.
  # Dividing by 15 years gives annual average emissions reductions.
  # Multiplying by 20 years applies these emissions reductions over the whole 20-year period from 2015 to 2035.
  # These relative emissions reduction need to be converted to a target factor by subtracting them from one.
  ExtrapolatedTarget <- setYears(1 - ((1 - ghgFactorExtrapolated[, 2030, ]) / 15 * 20), 2035)
  # only replace extrapolated 2035 target if country has no 2035 yet
  Target2035 <- ghgFactorExtrapolated[, 2035, ]
  Target2035[is.na(Target2035)] <- ExtrapolatedTarget[is.na(Target2035)]
  ghgFactorExtrapolated[, 2035, ] <- Target2035
  # add extrapolated 2025 NDC versions to input data
  ghgFactor <- mbind(ghgFactor, ghgFactorExtrapolated)

# 3. Aggregate country-level target factors to region-level target factors ----

    if (subtype == "Ghgfactor") {


# Explanation: The target factor ("ghgFactor") represents NDC target emissions normalized by 2015 emissions on country-level. The emissions
# cover total GHG emissions excl. land-use change and excl. bunker emissions. They are aggregated to region-level
# by a weighted sum of all countries with an NDC target where the weights are the 2015 emissions of the country
# normalized to the 2015 emissions of the region:
# targetFactor(region) = sum(country, emi2015(country) / emi2015(region) * targetFactor(country) ), for all countries with NDC targets.
# Note that his aggregation is done via the madrat routine run with the return() statement of this function.

    # target factor as aggregation variable
    x <- ghgFactor
    x[is.na(x)] <- 0

    # create 1/0 mask encoding whether a target year and country
    # has a target represented by a GHG factor
    mask <- 1 * !is.na(ghgFactor)

    # GHG emission as weight, but only for countries and years with a GHG factor
    weight <- setNames(setYears(ghg[, 2015, ], NULL), NULL) * mask


    return(list(
      x = x,
      weight = weight,
      unit = "1",
      description = glue::glue("Multiplier for target year emissions vs 2015 emissions, \\
                as weighted average for all countries with NDC target in each region per target year."),
      # do not throw warning for zero weights, as they only occur when there are no values to be aggregated
      aggregationArguments = list(zeroWeight = "allow")
    ))
  }

# 4. Calculate share of emissions covered under NDC target per REMIND region ----

  if (subtype == "Ghgshare") {

    # Explanation: The share of emissions covered under NDC ("ghgShare") is an estimate of target year emissions in a REMIND region
    # from all countries that have an NDC target. It is used to proxy which share of emissions in a region should follow NDC targets and
    # which should follow the Npi scenario (for countries without target).
    # There are two steps:
    # 1. Target year emissions on country-level are projected by assuming the same growth rate of emissions as GDP:
    # Emi(target year) = Emi(2015) * GDP(target year) / GDP(2015).
    # 2. The share of emissions from countries with an NDC target in the region is calculated as:
    # ghgShare = sum(country, Emi(target year, country) / Emi(target year, region) ) for all countries that have NDC targets
    # Note this calculation is done via the madrat aggregation routine run with the return() statement of this function.



    # aggregation variable: 0/1 matrix with 1s indicating countries with target represented as GHG factor
    x <- 1 * (!is.na(ghgFactor))

    # get GDP for extrapolating target year emissions needed for aggregation weights
    gdp <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)

    # estimate target year emissions by multiplying 2015 emissions with average GDP growth rate
    # use these target year emissions as aggregation weight
    weight <- new.magpie(getRegions(ghgFactor), getYears(ghgFactor), getNames(ghgFactor), fill = NA)
    weight[, , ] <- setYears(ghg[, 2015, ] / gdp[, 2015, ], NULL) * gdp[, getYears(weight), ]

    return(list(
      x = x,
      weight = weight,
      unit = "1",
      description = glue::glue("2015 GHG emission share of countries with \\
                quantifyable emissions under NDC in particular region per target year"),
      min = 0, max = 1
    ))
  }

}
