#' Calculate Emission Targets
#'
#' @param sources database source, either 'UNFCCC_NDC' or 'NewClimate'
#' @param subtype must be one of
#' - 'Ghgfactor': for GHG factors calculated from the respective database
#' - 'Ghgshare2015': for Emission share of countries with targets per region
#' @param scenario GDP and pop scenarios. Passed to [mrdrivers::calcGDP()].
#' turned off for inpudata generation
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Rahel Mandaroux, Falk Benke
#'
calcEmiTarget <- function(sources, subtype, scenario) {

  if (!sources %in% c("UNFCCC_NDC", "NewClimate")) {
    stop("Unknown source ", sources, " for calcEmiTarget.")
  }

  if (!subtype %in% c("Ghgshare2015", "Ghgfactor")) {
    stop("Unknown 'subtype' argument")
  }

  # Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs", to calls of the individual scenarios.
  scenario <- mrdrivers::toolReplaceShortcuts(scenario) %>% unique()

  # Reference Emissions from CEDS
  emi <- calcOutput("EmiTargetReference", aggregate = FALSE)
  ghg <- emi[, , "Emi|GHG|w/o Bunkers|w/o Land-Use Change (Mt CO2eq/yr)"]

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

  # ensure that all magclass objects in the list have matching years so they can be bound together

  listYears <- lapply(listGhgFactors, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
  ghgFactor <- purrr::map(listGhgFactors,
                          ~ add_columns(.x, listYears[!listYears %in% getItems(.x, dim = "year")], 2))

  ghgFactor <- mbind(ghgFactor)
  ghgFactor <- ghgFactor[, sort(getYears(ghgFactor)), ]

  # GHG factors weighted by GHG in 2015
  if (subtype == "Ghgfactor") {

  # 2. Extrapolate NDC targets from 2030 to 2035 for countries which do not have 2035 NDC targets ----

  # add NDC versions "Emissions_2025_cond_extrapol" and "Emissions_2025_uncond_extrapol"
  # that take the 2035 targets from the 2025 version, but extrapolate 2030 targets to 2035 for countries which do not have 2035 targets yet
  # create new NDC version 2025 with extrapolated targets

  # if no 2035 targets available, add 2035 to target factor object
  if (! "y2035" %in% getYears(ghgFactor)) {
    ghgFactor <- mbind(ghgFactor, new.magpie(getRegions(ghgFactor),"y2035",getNames(ghgFactor), fill = NA))
  }
  ghgFactorExtrapolated <- ghgFactor[,,c("2025_cond","2025_uncond")]
  getItems(ghgFactorExtrapolated, dim=3.1) <- paste0(getItems(ghgFactorExtrapolated, dim=3.1),"_extrapol")
  # loop over countries and NDC scenarios to extrapolate 2030 targets to 2035 targets for those countries who do not have 2035 targets
  for (country in getRegions(ghgFactorExtrapolated ) ) {
    for (NDCscenario in getItems(ghgFactorExtrapolated, dim=3) ) {
      # do extrapolation only if country has 2030 target but no 2035 target
      if (c("y2035") %in% getYears(ghgFactorExtrapolated)) {
        Do2035Extrapolation <- ( as.vector(is.na(ghgFactorExtrapolated[country, "y2035", NDCscenario])) &
                                 as.vector(!is.na(ghgFactorExtrapolated[country, "y2030", NDCscenario])))
      } else {
        Do2035Extrapolation <- as.vector(!is.na(ghgFactorExtrapolated[country, "y2030", NDCscenario]))
      }

      # if country has 2030 target but no 2035 target
      if (Do2035Extrapolation) {
        # if no 2035 target available, linearly extrapolate target from 2030 to 2035
        # Explanation of the calculation:
        # Note that the ghgFactor gives the remaining relative emissions relative to 2015.
        # Hence, 1 - ghgFactor gives the relative emissions reductions relative to 2015.
        # Dividing by 15 years gives annual average emissions reductions.
        # Multiplying by 20 years applies these emissions reductions over the whole 20-year period from 2015 to 2035.
        # These relative emissions reduction need to be converted to a target factor by subtracting them from one.
        ghgFactorExtrapolated[country, "y2035", NDCscenario] <- 1 - ( (1 - ghgFactorExtrapolated[country, "y2030", NDCscenario]) / 15 * 20)

        print(paste0(country, " has no 2035 NDC emissions target."))
        print(paste0("For the scenario ", NDCscenario, ", linearly extrapolate target factor from 2030 to 2035: ", ghgFactorExtrapolated[country, "y2030", NDCscenario], " -> ", ghgFactorExtrapolated[country, "y2035", NDCscenario]))
      }
    }
  }

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

  if (subtype == "Ghgshare2015") {

    # 0/1 matrix with 1s indicating countries with target represented as GHG factor
    x <- 1 * (!is.na(ghgFactor))

    gdp <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)

    # calculate growth for GDP weight for GHG emission share
    # assuming constant relative emission intensities across countries of one region
    weight <- ghgFactor
    weight[, , ] <- NA
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
