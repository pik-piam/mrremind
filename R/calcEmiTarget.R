#' Calculate NDC Emissions Targets
#'
#' @description This function calculates the emissions targets for the NDC scenarios applied in the REMIND module 45_carbonprice realization NDC.
#' It contains the following steps:
# 1. Read country-level NDC targets as absolute emissions targets in MtCO2eq/yr
# 2. Make country-specific assumptions about inclusions or adaptations of NDC targets
# 3  Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets (yet)
# 4. Aggregate country-level absolute emissions targets to region-level absolute emissions targets by summation ("EmiTargetAbs")
# 5. Calculate share of emissions covered under NDC target per REMIND region ("Ghgshare")
#' The parameters calculated in 3.) and 4.) are further used in the NDC realization to calculate the region-wide NDC emissions targets
#' in terms of total GHG emissions excl. bunkers and excl. LULUCF sectors.
#'
#' @param sources database source, either 'UNFCCC_NDC' or 'NewClimate'
#' @param subtype must be one of
#' - 'EmiTargetAbs': absolute emissions targets in MtCO2eq/yr
#' - 'Ghgshare': share of emissions covered under NDC target per REMIND region
#' @param scenario GDP and pop scenarios. Passed to [mrdrivers::calcGDP()].
#' turned off for inpudata generation
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Rahel Mandaroux, Falk Benke
#'
calcEmiTarget <- function(sources, subtype, scenario) {
  # Main steps:
  # 1. Read country-level NDC targets as absolute emissions targets in MtCO2eq/yr
  # 2. Make country-specific assumptions about inclusions or adaptations of NDC targets
  # 3  Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets (yet)
  # 4. Aggregate country-level absolute emissions targets to region-level absolute emissions targets by summation ("EmiTargetAbs")
  # 5. Calculate share of emissions covered under NDC target per REMIND region ("Ghgshare")


  # 1. Read country-level NDC targets as absolute emissions targets ----

  # check whether valid sources and subtypes chosen
  if (!sources %in% c("UNFCCC_NDC", "NewClimate")) {
    stop("Unknown source ", sources, " for calcEmiTarget.")
  }

  if (!subtype %in% c("Ghgshare", "EmiTargetAbs")) {
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
      "2025_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2025_uncond", subset = scenario),
      "2026_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2026_cond", subset = scenario)
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
  listYears <- lapply(listGhgFactors, getItems, dim = "year") %>%
    unlist() %>%
    unique() %>%
    sort()
  absEmiTarget <- purrr::map(
    listGhgFactors,
    ~ add_columns(.x, listYears[!listYears %in% getItems(.x, dim = "year")], 2)
  )
  absEmiTarget <- mbind(absEmiTarget)
  absEmiTarget <- absEmiTarget[, sort(getYears(absEmiTarget)), ]

  # 2. Make country-specific assumptions about inclusions or adaptations of NDC targets ----

  # remove US targets from NDC targets as of 2024
  # since under the Trump Administration the US has started a process of withdrawing from the Paris Agreement
  if (sources == "UNFCCC_NDC") {
  absEmiTarget["USA",,c("2024_uncond","2024_cond","2025_uncond","2025_cond", "2026_cond")] <- NA }


  # 3. Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets ----

  # for NDC version 2026 that includes 2030 and 2035 targets
  # extrapolate 2030 targets to 2035 for countries which do not have 2035 targets yet
  NDCVersionToExtrapolate <- grep("2026", getNames(absEmiTarget, dim=1), value=T)

  if (length(NDCVersionToExtrapolate) > 0) {
    # if no 2035 targets available, add 2035 to target object
    if (!2035 %in% getYears(absEmiTarget, as.integer = TRUE)) {
      absEmiTarget <- add_columns(absEmiTarget, addnm = "y2035", dim = 2, fill = NA)
    }
    # create magclass object for extrapolated NDC scenarios
    absEmiTargetExtrapolated <- absEmiTarget[, , NDCVersionToExtrapolate ]
    
    # Load 2015 reference emissions for extrapolation
    ghgRef2015 <- setYears(ghg[, 2015, ], NULL)
    
    # Explanation of the extrapolation:
    # For absolute emissions targets, we extrapolate from 2030 to 2035 using linear extrapolation
    # based on the annual reduction rate from 2015 to 2030:
    # Annual reduction rate = (2030 target - 2015 emissions) / 15 years
    # 2035 target = 2030 target + (annual reduction rate × 5 years)
    annualReductionRate <- (absEmiTargetExtrapolated[, 2030, ] - ghgRef2015) / 15
    ExtrapolatedTarget2035 <- setYears(absEmiTargetExtrapolated[, 2030, ] + annualReductionRate * 5, 2035)
    
    # only replace extrapolated 2035 target if country has no 2035 yet
    Target2035 <- absEmiTargetExtrapolated[, 2035, ]
    Target2035[is.na(Target2035)] <- ExtrapolatedTarget2035[is.na(Target2035)]
    absEmiTargetExtrapolated[, 2035, ] <- Target2035
    # add extrapolated 2026 NDC versions to input data
    absEmiTarget <- mbind(absEmiTarget[,,setdiff(getNames(absEmiTarget, dim=1),NDCVersionToExtrapolate)], absEmiTargetExtrapolated)
  }


  # 4. Aggregate country-level absolute emissions targets to region-level absolute emissions targets ----

  if (subtype == "EmiTargetAbs") {
    # Explanation: The absolute emissions target ("absEmiTarget") represents NDC target emissions in MtCO2eq/yr on country-level.
    # The emissions cover total GHG emissions excl. land-use change and excl. bunker emissions.
    # They are aggregated to region-level by simple summation of all countries with an NDC target:
    # absEmiTarget(region) = sum(country, absEmiTarget(country)), for all countries with NDC targets.
    # Note that this aggregation is done via the madrat routine run with the return() statement of this function.
    # Weight is set to NULL to sum all country-level targets without weights.

    # absolute emissions target as aggregation variable
    x <- absEmiTarget
    # set countries without NDC targets to 0
    # their contribution to the regional NDC target is taken care of
    # in the GAMS code of "./modules/45_carbonprice/NDC/." by adding their share of emissions from the NPI run
    x[is.na(x)] <- 0



    return(list(
      x = x,
      weight = NULL,
      unit = "MtCO2eq/yr",
      description = glue::glue("Absolute emissions targets in MtCO2eq/yr, \\
                summed for all countries with NDC target in each region per target year.")
    ))
  }

  # 5. Calculate share of emissions covered under NDC target per REMIND region ----

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



    # aggregation variable: 0/1 matrix with 1s indicating countries with target represented as absolute emissions target
    x <- 1 * (!is.na(absEmiTarget))

    # get GDP for extrapolating target year emissions needed for aggregation weights
    gdp <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)

    # estimate target year emissions by multiplying 2015 emissions with average GDP growth rate
    # use these target year emissions as aggregation weight
    weight <- new.magpie(getRegions(absEmiTarget), getYears(absEmiTarget), getNames(absEmiTarget), fill = NA)
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
