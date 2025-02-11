#' Output for 2 policy cases
#' @param subtype A string, one of: "Ghgshare2005", "Ghgfactor", "Ghghistshare"
#' @param scenario GDP and pop scenarios. Passed to [mrdrivers::calcGDP()].
#' @param sources Database source
#'
calcEmiTarget <- function(sources, subtype, scenario) {
  if (!sources %in% c("UNFCCC_NDC", "NewClimate")) {
    stop("Unknown source ", sources, " for calcEmiTarget.")
  }
  if (!subtype %in% c("Ghgshare2005", "Ghgfactor", "Ghghistshare")) {
    stop("Unknown 'subtype' argument")
  }

  ## Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs", to calls of the individual scenarios.
  scenario <- mrdrivers::toolReplaceShortcuts(scenario) %>% unique()

  # Import historical emi needed for the calculations
  ## Historical emissions for 1990-2015 - co2 (excl LU),ch4,n2o (so far no Fgas historic time series)
  ceds <- calcOutput("Emissions", datasource = "CEDS2REMIND", years = 1990:2015, aggregate = FALSE)
  ## Calculate GHG total of CO2, CH4 and N2O [unit Mt CO2eq]
  ## Global Warming Potentials of CH4 and N20, AR5 WG1 CH08 Table 8.7
  gwpCH4 <- 28
  gwpN2O <- 265
  ## Note: CEDS2024 does not include 'Emi|N2O|Land Use|*' variables and cannot be used.
  n2Ovars <- c("Emi|N2O|Energy and Industrial Processes (kt N2O/yr)",
               "Emi|N2O|Land Use|Agriculture and Biomass Burning (kt N2O/yr)",
               "Emi|N2O|Land Use|Forest Burning (kt N2O/yr)",
               "Emi|N2O|Land Use|Grassland Burning (kt N2O/yr)",
               "Emi|N2O|Waste (kt N2O/yr)")
  ch4vars <- c("Emi|CH4|Energy and Industrial Processes (Mt CH4/yr)",
               "Emi|CH4|Land Use|Agriculture and Biomass Burning (Mt CH4/yr)",
               "Emi|CH4|Land Use|Forest Burning (Mt CH4/yr)",
               "Emi|CH4|Land Use|Grassland Burning (Mt CH4/yr)",
               "Emi|CH4|Waste (Mt CH4/yr)")
  ghg <- ceds[, , c("Emi|CO2|Energy and Industrial Processes (Mt CO2/yr)")] +
    gwpN2O / 1000 * dimSums(ceds[, , n2Ovars], dim = 3) +
    gwpCH4 * dimSums(ceds[, , ch4vars], dim = 3)
  ## Create global data for checking plausibility of data
  globGhg <- dimSums(ghg, dim = 1)
  ghg <- toolCountryFill(ghg, fill = 0, verbosity = 2)

  # Make sure SSP2 is included in the ghgFactor scenarios. Required for the computations.
  subsetScen <- unique(c(scenario, "SSP2"))
  # Get ghgfactor
  if (sources == "UNFCCC_NDC") {
    listGhgfactors <- list(
      "2018_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2018_cond", subset = subsetScen),
      "2018_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2018_uncond", subset = subsetScen),
      "2021_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2021_cond", subset = subsetScen),
      "2021_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2021_uncond", subset = subsetScen),
      "2022_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2022_cond", subset = subsetScen),
      "2022_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2022_uncond", subset = subsetScen),
      "2023_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2023_cond", subset = subsetScen),
      "2023_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2023_uncond", subset = subsetScen),
      "2024_cond"   = readSource("UNFCCC_NDC", subtype = "Emissions_2024_cond", subset = subsetScen),
      "2024_uncond" = readSource("UNFCCC_NDC", subtype = "Emissions_2024_uncond", subset = subsetScen)
    )
  }

  if (sources == "NewClimate") {
    listGhgfactors <- list(
      "2025_cond"   = readSource("NewClimate", subtype = "Emissions_2025_cond"),
      "2025_uncond" = readSource("NewClimate", subtype = "Emissions_2025_uncond")
    )
  }

  listYears <- lapply(listGhgfactors, getItems, dim = "year") %>% unlist() %>% unique() %>% sort()
  ghgfactor <- purrr::map(listGhgfactors,
                           ~ add_columns(.x, listYears[!listYears %in% getItems(.x, dim = "year")], 2)) %>%
    mbind()
  ghgfactor <- ghgfactor[, sort(getYears(ghgfactor)), ]

  # create 1/0 dummy for calculation of regional share covered by quantitative target, per TarYear.
  # Note that 0 implies no goal, net zero targets have ghgfactor of 0 but dummy of 1
  dummy1 <- 1 * !is.na(ghgfactor[, , "SSP2", drop = TRUE])
  ghgfactor <- mselect(ghgfactor, "data" = scenario)

  if (subtype == "Ghgfactor") {
    # in order to calculate the share of regional emissions coming from countries with quantitative target
    ghgTarget <- setNames(setYears(ghg[, 2005, ], NULL), NULL) * dummy1[, , ]
    description <- glue::glue("Multiplier for target year emissions vs 2005 emissions, as weighted average for \\
                               all countries with NDC target in each region per target year")
    x <- ghgfactor
    x[is.na(x)] <- 0
    # Min max are not necessary fixed values defined until eternity, but rather plausibility checks
    return(list(x = x, weight = ghgTarget, unit = "1", description = description, min = -5, max = 4))
  }

  if (subtype == "Ghgshare2005") {
    # Get gdp scenarios
    gdp <- calcOutput("GDP", scenario = scenario, aggregate = FALSE)
    # calculate growth for GDP weight for GHG emission share
    # assuming constant relative emission intensities across countries of one region
    gdpWeight <- new.magpie(getItems(dummy1, dim = "iso3c"), getItems(dummy1, dim = "year"), getNames(ghgfactor))
    for (t in getItems(dummy1, dim = "year")) {
      gdpWeight[, t, ] <- setYears(ghg[, 2005, ] / gdp[, 2005, ], NULL) * gdp[, t, ]
    }
    description <- glue::glue("2005 GHG emission share of countries with quantifyable emissions under NDC in \\
                                particular region per target year")
    return(list(x = 1 * (!is.na(ghgfactor[, , ])),
                weight = gdpWeight,
                unit = "1",
                description = description,
                min = 0,
                max = 1))
  }

  if (subtype == "Ghghistshare") {
    # make ghgTarget only represent countries with 2030 data
    dummy2 <- new.magpie(cells_and_regions = getItems(ghg, dim = "region"),
                         years = getItems(ghg, dim = "year"),
                         names = names(listGhgfactors))
    dummy2[, , ] <- dummy1[, "y2030", ]
    description <- "GHG emissions share of countries with quantifyable 2030 target in particular region"
    return(list(x = dummy2, weight = ghg, unit = "1", description = description, min = 0, max = 1))
  }
}
