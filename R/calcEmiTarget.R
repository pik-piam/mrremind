#' Calculate Emission Targets
#'
#' TODO: describe what this is used for in REMIND and document the meaning of the subtypes
#'
#' @param sources database source, either 'UNFCCC_NDC' or 'NewClimate'
#' @param subtype must be one of
#' - 'Ghgfactor': for GHG factors calculated from the respective database
#' - 'Ghgshare2005': ???
#' @param scenario GDP and pop scenarios. Passed to [mrdrivers::calcGDP()].
#' @param verbose set to TRUE for additional info on processing of NDC target,
#' turned off for inpudata generation
#' @author Aman Malik, Christoph Bertram, Oliver Richters, Rahel Mandaroux, Falk Benke
#'
calcEmiTarget <- function(sources, subtype, scenario, verbose = TRUE) {

  if (!sources %in% c("UNFCCC_NDC", "NewClimate")) {
    stop("Unknown source ", sources, " for calcEmiTarget.")
  }

  if (!subtype %in% c("Ghgshare2005", "Ghgfactor")) {
    stop("Unknown 'subtype' argument")
  }

  # Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs", to calls of the individual scenarios.
  scenario <- mrdrivers::toolReplaceShortcuts(scenario) %>% unique()


  # Reference Emissions from CEDS
  ghg <- calcOutput("EmiTargetReference", aggregate = FALSE)

  .read <- function(src, subtype, subset, verbose) {
    if (verbose) {
      x <- readSource(src, subtype, subset)
    } else {
      x <- suppressMessages(readSource(src, subtype, subset))
    }
    return(x)
  }

  if (sources == "UNFCCC_NDC") {
    listGhgFactors <- list(
      "2018_cond"   = .read("UNFCCC_NDC", subtype = "Emissions_2018_cond", subset = scenario, verbose = verbose),
      "2018_uncond" = .read("UNFCCC_NDC", subtype = "Emissions_2018_uncond", subset = scenario, verbose = verbose),
      "2021_cond"   = .read("UNFCCC_NDC", subtype = "Emissions_2021_cond", subset = scenario, verbose = verbose),
      "2021_uncond" = .read("UNFCCC_NDC", subtype = "Emissions_2021_uncond", subset = scenario, verbose = verbose),
      "2022_cond"   = .read("UNFCCC_NDC", subtype = "Emissions_2022_cond", subset = scenario, verbose = verbose),
      "2022_uncond" = .read("UNFCCC_NDC", subtype = "Emissions_2022_uncond", subset = scenario, verbose = verbose),
      "2023_cond"   = .read("UNFCCC_NDC", subtype = "Emissions_2023_cond", subset = scenario, verbose = verbose),
      "2023_uncond" = .read("UNFCCC_NDC", subtype = "Emissions_2023_uncond", subset = scenario, verbose = verbose),
      "2024_cond"   = .read("UNFCCC_NDC", subtype = "Emissions_2024_cond", subset = scenario, verbose = verbose),
      "2024_uncond" = .read("UNFCCC_NDC", subtype = "Emissions_2024_uncond", subset = scenario, verbose = verbose)
    )
  }

  if (sources == "NewClimate") {
    listGhgFactors <- list(
      "2025_cond"   = .read("NewClimate", subtype = "Emissions_2025_cond", subset = scenario, verbose = verbose),
      "2025_uncond" = .read("NewClimate", subtype = "Emissions_2025_uncond", subset = scenario, verbose = verbose)
    )
  }

  # ensure that all magclass objects in the list have matching years so they can be bound together
  listYears <- lapply(listGhgFactors, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
  ghgFactor <- purrr::map(listGhgFactors,
                          ~ add_columns(.x, listYears[!listYears %in% getItems(.x, dim = "year")], 2))

  ghgFactor <- mbind(ghgFactor)
  ghgFactor <- ghgFactor[, sort(getYears(ghgFactor)), ]

  # GHG factors weighted by GHG in 2005
  if (subtype == "Ghgfactor") {

    x <- ghgFactor
    x[is.na(x)] <- 0

    # create 1/0 mask encoding whether a target year and country
    # has a target represented by a GHG factor
    mask <- 1 * !is.na(ghgFactor)

    # GHG emission as weight, but only for countries and years with a GHG factor
    weight <- setNames(setYears(ghg[, 2005, ], NULL), NULL) * mask

    return(list(
      x = x,
      weight = weight,
      unit = "1",
      description = glue::glue("Multiplier for target year emissions vs 2005 emissions, \\
                as weighted average for all countries with NDC target in each region per target year."),
      # TODO: this might have to be adjusted once the new bounds are set
      min = -5, max = 4,
      # do not throw warning for zero weights, as they only occur when there are no values to be aggregated
      aggregationArguments = list(zeroWeight = "allow")
    ))
  }

  # TODO: update to 2015 or even 2020
  if (subtype == "Ghgshare2005") {

    # 0/1 matrix with 1s indicating countries with target represented as GHG factor
    x <- 1 * (!is.na(ghgFactor))

    gdp <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)

    # calculate growth for GDP weight for GHG emission share
    # assuming constant relative emission intensities across countries of one region
    weight <- ghgFactor
    weight[, , ] <- NA
    weight[, , ] <- setYears(ghg[, 2005, ] / gdp[, 2005, ], NULL) * gdp[, getYears(weight), ]

    return(list(
      x = x,
      weight = weight,
      unit = "1",
      description = glue::glue("2005 GHG emission share of countries with \\
                quantifyable emissions under NDC in particular region per target year"),
      min = 0, max = 1
    ))
  }

}
