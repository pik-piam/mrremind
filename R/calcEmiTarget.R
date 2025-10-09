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
      "2024_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2024_uncond", subset = scenario)
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
